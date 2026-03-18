// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ssl/ssl_backend_openssl.cpp
//!
//! OpenSSL-backed SSL connection backend. Uses the `openssl` crate to provide
//! TLS client connections, matching the upstream OpenSSL backend's behavior.
//!
//! Key differences from upstream:
//! - Uses the Rust `openssl` crate's safe wrappers instead of raw C OpenSSL API.
//! - Uses `SslStream<TcpStreamAdapter>` instead of custom BIO callbacks.
//!   The upstream custom BIO approach is needed because they use their own
//!   socket abstraction (Network::SocketBase). Here we adapt the socket FD
//!   into a TcpStream and use openssl's built-in stream integration.
//! - SSLKEYLOGFILE support uses `SslContextBuilder::set_keylog_callback`.

use std::io::{self, Read as IoRead, Write as IoWrite};
use std::net::TcpStream;
use std::sync::Once;

use openssl::ssl::{SslConnector, SslMethod, SslStream, SslVerifyMode};

use crate::hle::result::ResultCode;

use super::ssl_backend::{SslConnectionBackend, RESULT_INTERNAL_ERROR, RESULT_WOULD_BLOCK};

// =========================================================================
// One-time initialization
// =========================================================================

static ONE_TIME_INIT: Once = Once::new();
static mut ONE_TIME_INIT_SUCCESS: bool = false;

/// One-time OpenSSL initialization.
///
/// Corresponds to `OneTimeInit()` in upstream ssl_backend_openssl.cpp.
/// The `openssl` crate handles library initialization automatically.
fn one_time_init() {
    unsafe {
        ONE_TIME_INIT_SUCCESS = true;
    }
    log::info!("OpenSSL one-time initialization succeeded");
}

fn is_initialized() -> bool {
    ONE_TIME_INIT.call_once(one_time_init);
    // SAFETY: ONE_TIME_INIT_SUCCESS is only written inside call_once, which
    // guarantees happens-before with all subsequent reads.
    unsafe { ONE_TIME_INIT_SUCCESS }
}

// =========================================================================
// TcpStreamAdapter — wraps a raw FD for use with SslStream
// =========================================================================

/// Wraps a duplicated TcpStream for use with OpenSSL's SslStream.
///
/// In upstream, custom BIO callbacks (ReadCallback, WriteCallback, CtrlCallback)
/// bridge OpenSSL to the emulator's Network::SocketBase. Here we duplicate the
/// socket FD into a TcpStream, which gives us standard Read/Write impls that
/// OpenSSL's SslStream can use directly.
struct TcpStreamAdapter {
    stream: TcpStream,
}

impl TcpStreamAdapter {
    /// Create an adapter by duplicating the given file descriptor.
    ///
    /// Corresponds to upstream `SetSocket(shared_ptr<SocketBase> socket)` which
    /// stores the socket reference. Here we dup the FD to avoid taking ownership.
    #[cfg(unix)]
    fn from_fd(fd: i32) -> io::Result<Self> {
        use std::os::fd::FromRawFd;
        let dup_fd = unsafe { libc::dup(fd) };
        if dup_fd < 0 {
            return Err(io::Error::last_os_error());
        }
        let stream = unsafe { TcpStream::from_raw_fd(dup_fd) };
        Ok(Self { stream })
    }

    #[cfg(not(unix))]
    fn from_fd(_fd: i32) -> io::Result<Self> {
        Err(io::Error::new(
            io::ErrorKind::Unsupported,
            "TcpStreamAdapter::from_fd is only supported on Unix",
        ))
    }
}

impl IoRead for TcpStreamAdapter {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.stream.read(buf)
    }
}

impl IoWrite for TcpStreamAdapter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.stream.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.stream.flush()
    }
}

// =========================================================================
// SSLConnectionBackendOpenSSL
// =========================================================================

/// OpenSSL-backed SSL connection.
///
/// Corresponds to `SSLConnectionBackendOpenSSL` in upstream
/// ssl_backend_openssl.cpp.
struct SslConnectionBackendOpenSsl {
    /// The SSL stream, created during handshake.
    ssl_stream: Option<SslStream<TcpStreamAdapter>>,
    /// The raw TCP adapter, held before handshake connects it.
    tcp_adapter: Option<TcpStreamAdapter>,
    /// Hostname for SNI and verification.
    hostname: Option<String>,
    /// Whether we got a read EOF from the peer.
    ///
    /// Corresponds to `got_read_eof` in upstream.
    got_read_eof: bool,
}

impl SslConnectionBackendOpenSsl {
    fn new() -> Self {
        Self {
            ssl_stream: None,
            tcp_adapter: None,
            hostname: None,
            got_read_eof: false,
        }
    }
}

impl SslConnectionBackend for SslConnectionBackendOpenSsl {
    /// SetSocket.
    ///
    /// Corresponds to `SSLConnectionBackendOpenSSL::SetSocket` in upstream.
    fn set_socket(&mut self, socket_fd: i32) {
        log::debug!("SSLConnectionBackendOpenSSL::SetSocket: fd={}", socket_fd);
        match TcpStreamAdapter::from_fd(socket_fd) {
            Ok(adapter) => {
                self.tcp_adapter = Some(adapter);
            }
            Err(e) => {
                log::error!("Failed to dup socket fd {}: {}", socket_fd, e);
            }
        }
    }

    /// SetHostName.
    ///
    /// Corresponds to upstream `SSL_set1_host` (for verification) and
    /// `SSL_set_tlsext_host_name` (for SNI). Both are handled by the
    /// `openssl` crate's `SslConnector::connect(hostname, ...)`.
    fn set_host_name(&mut self, hostname: &str) -> ResultCode {
        log::debug!("SSLConnectionBackendOpenSSL::SetHostName: {}", hostname);
        self.hostname = Some(hostname.to_string());
        ResultCode(0) // RESULT_SUCCESS
    }

    /// DoHandshake.
    ///
    /// Corresponds to `SSLConnectionBackendOpenSSL::DoHandshake` in upstream.
    /// Upstream sequence:
    ///   SSL_set_verify_result(ssl, X509_V_OK)
    ///   SSL_do_handshake(ssl)
    ///   SSL_get_verify_result(ssl) — check cert verification
    ///   HandleReturn("SSL_do_handshake", 0, ret)
    fn do_handshake(&mut self) -> ResultCode {
        log::debug!("SSLConnectionBackendOpenSSL::DoHandshake called");

        if !is_initialized() {
            log::error!(
                "Can't create SSL connection because OpenSSL one-time initialization failed"
            );
            return RESULT_INTERNAL_ERROR;
        }

        let tcp_adapter = match self.tcp_adapter.take() {
            Some(a) => a,
            None => {
                log::error!("DoHandshake called without a socket");
                return RESULT_INTERNAL_ERROR;
            }
        };

        let hostname = self.hostname.as_deref().unwrap_or("localhost");

        // Build the SSL connector.
        //
        // Upstream: SSL_CTX_new(TLS_client_method()), SSL_CTX_set_verify(SSL_VERIFY_PEER),
        // SSL_CTX_set_default_verify_paths()
        let connector = match SslConnector::builder(SslMethod::tls_client()) {
            Ok(mut builder) => {
                builder.set_verify(SslVerifyMode::PEER);

                // Corresponds to upstream SSL_CTX_set_default_verify_paths
                if let Err(e) = builder.set_default_verify_paths() {
                    log::error!("set_default_verify_paths failed: {}", e);
                    self.tcp_adapter = Some(tcp_adapter);
                    return RESULT_INTERNAL_ERROR;
                }

                // SSLKEYLOGFILE support.
                // Corresponds to upstream OneTimeInitLogFile + KeyLogCallback.
                if let Ok(logfile) = std::env::var("SSLKEYLOGFILE") {
                    builder.set_keylog_callback(move |_ssl, line| {
                        use std::io::Write;
                        if let Ok(mut f) = std::fs::OpenOptions::new()
                            .append(true)
                            .create(true)
                            .open(&logfile)
                        {
                            let _ = writeln!(f, "{}", line);
                            let _ = f.flush();
                        } else {
                            log::error!("Failed to write to SSLKEYLOGFILE");
                        }
                        log::debug!("Wrote to SSLKEYLOGFILE: {}", line);
                    });
                }

                builder.build()
            }
            Err(e) => {
                log::error!("SslConnector::builder failed: {}", e);
                self.tcp_adapter = Some(tcp_adapter);
                return RESULT_INTERNAL_ERROR;
            }
        };

        // Perform the handshake.
        //
        // SslConnector::connect handles:
        //   SSL_set1_host (hostname verification)
        //   SSL_set_tlsext_host_name (SNI)
        //   SSL_do_handshake
        //   SSL_get_verify_result
        match connector.connect(hostname, tcp_adapter) {
            Ok(stream) => {
                log::info!("SSL handshake succeeded for {}", hostname);
                self.ssl_stream = Some(stream);
                ResultCode(0) // RESULT_SUCCESS
            }
            Err(openssl::ssl::HandshakeError::WouldBlock(_mid)) => {
                // Corresponds to upstream HandleReturn returning ResultWouldBlock
                // for SSL_ERROR_WANT_READ / SSL_ERROR_WANT_WRITE.
                log::debug!("SSL handshake would block");
                RESULT_WOULD_BLOCK
            }
            Err(openssl::ssl::HandshakeError::Failure(mid)) => {
                let ssl_error = mid.into_error();
                // Corresponds to upstream checking SSL_get_verify_result and
                // X509_verify_cert_error_string.
                log::error!("SSL handshake failed: {}", ssl_error);
                RESULT_INTERNAL_ERROR
            }
            Err(openssl::ssl::HandshakeError::SetupFailure(e)) => {
                log::error!("SSL handshake setup failure: {}", e);
                RESULT_INTERNAL_ERROR
            }
        }
    }

    /// Read.
    ///
    /// Corresponds to `SSLConnectionBackendOpenSSL::Read` in upstream.
    /// Upstream: SSL_read_ex(ssl, data, size, &actual) then HandleReturn.
    fn read(&mut self, data: &mut [u8]) -> Result<usize, ResultCode> {
        let stream = self.ssl_stream.as_mut().ok_or(RESULT_INTERNAL_ERROR)?;

        match stream.ssl_read(data) {
            Ok(0) => {
                // SSL_ERROR_ZERO_RETURN — peer sent close_notify.
                // Upstream HandleReturn: *actual = 0; return ResultSuccess.
                self.got_read_eof = true;
                Ok(0)
            }
            Ok(n) => Ok(n),
            Err(e) => {
                match e.into_io_error() {
                    Ok(err) if err.kind() == io::ErrorKind::WouldBlock => {
                        // SSL_ERROR_WANT_READ or SSL_ERROR_WANT_WRITE
                        Err(RESULT_WOULD_BLOCK)
                    }
                    Ok(err) => {
                        // SSL_ERROR_SYSCALL with got_read_eof => return success(0)
                        if self.got_read_eof {
                            log::debug!("SSL read: SSL_ERROR_SYSCALL because server hung up");
                            Ok(0)
                        } else {
                            log::error!("SSL read error: {}", err);
                            Err(RESULT_INTERNAL_ERROR)
                        }
                    }
                    Err(_ssl_err) => {
                        // SSL protocol error (SSL_ERROR_ZERO_RETURN, etc.)
                        log::debug!("SSL read: connection closed");
                        self.got_read_eof = true;
                        Ok(0)
                    }
                }
            }
        }
    }

    /// Write.
    ///
    /// Corresponds to `SSLConnectionBackendOpenSSL::Write` in upstream.
    /// Upstream: SSL_write_ex(ssl, data, size, &actual) then HandleReturn.
    fn write(&mut self, data: &[u8]) -> Result<usize, ResultCode> {
        let stream = self.ssl_stream.as_mut().ok_or(RESULT_INTERNAL_ERROR)?;

        match stream.ssl_write(data) {
            Ok(n) => Ok(n),
            Err(e) => match e.into_io_error() {
                Ok(err) if err.kind() == io::ErrorKind::WouldBlock => Err(RESULT_WOULD_BLOCK),
                Ok(err) => {
                    log::error!("SSL write error: {}", err);
                    Err(RESULT_INTERNAL_ERROR)
                }
                Err(_ssl_err) => {
                    log::error!("SSL write: protocol error");
                    Err(RESULT_INTERNAL_ERROR)
                }
            },
        }
    }

    /// GetServerCerts.
    ///
    /// Corresponds to `SSLConnectionBackendOpenSSL::GetServerCerts` in upstream.
    /// Upstream: SSL_get_peer_cert_chain, then i2d_X509 for each cert to get
    /// DER-encoded bytes.
    fn get_server_certs(&self) -> Result<Vec<Vec<u8>>, ResultCode> {
        let stream = self.ssl_stream.as_ref().ok_or(RESULT_INTERNAL_ERROR)?;

        let ssl = stream.ssl();
        let chain = ssl.peer_cert_chain().ok_or_else(|| {
            log::error!("SSL_get_peer_cert_chain returned None");
            RESULT_INTERNAL_ERROR
        })?;

        let mut out_certs = Vec::new();
        for cert in chain.iter() {
            match cert.to_der() {
                Ok(der) => out_certs.push(der),
                Err(e) => {
                    log::error!("Failed to DER-encode certificate: {}", e);
                    // Upstream: ASSERT_OR_EXECUTE(... , { continue; })
                    continue;
                }
            }
        }

        Ok(out_certs)
    }
}

/// Create an OpenSSL-backed SSL connection backend.
///
/// Corresponds to `CreateSSLConnectionBackend` in upstream ssl_backend_openssl.cpp.
pub fn create_ssl_connection_backend() -> Result<Box<dyn SslConnectionBackend>, ResultCode> {
    if !is_initialized() {
        log::error!(
            "Can't create SSL connection because OpenSSL one-time initialization failed"
        );
        return Err(RESULT_INTERNAL_ERROR);
    }

    let backend = SslConnectionBackendOpenSsl::new();
    Ok(Box::new(backend))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn openssl_backend_creates_successfully() {
        let result = create_ssl_connection_backend();
        assert!(result.is_ok(), "OpenSSL backend should create successfully");
    }

    #[test]
    fn set_hostname_succeeds() {
        let mut backend = create_ssl_connection_backend().unwrap();
        let rc = backend.set_host_name("example.com");
        assert!(rc.is_success());
    }

    #[test]
    fn handshake_without_socket_returns_error() {
        let mut backend = create_ssl_connection_backend().unwrap();
        backend.set_host_name("example.com");
        let rc = backend.do_handshake();
        assert!(rc.is_error());
    }

    #[test]
    fn read_without_handshake_returns_error() {
        let mut backend = create_ssl_connection_backend().unwrap();
        let mut buf = [0u8; 16];
        let result = backend.read(&mut buf);
        assert!(result.is_err());
    }

    #[test]
    fn get_server_certs_without_handshake_returns_error() {
        let backend = create_ssl_connection_backend().unwrap();
        let result = backend.get_server_certs();
        assert!(result.is_err());
    }
}
