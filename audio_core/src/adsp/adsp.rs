use crate::adsp::apps::audio_renderer::AudioRenderer;
use crate::adsp::apps::opus::{Direction, Message, OpusDecoder};
use crate::sink::SinkHandle;
use crate::SharedSystem;
use parking_lot::Mutex;
use std::sync::Arc;

pub type AudioRendererHandle = Arc<Mutex<AudioRenderer>>;
pub type OpusDecoderHandle = Arc<Mutex<OpusDecoder>>;

pub struct ADSP {
    audio_renderer: AudioRendererHandle,
    opus_decoder: OpusDecoderHandle,
}

impl ADSP {
    pub fn new(system: SharedSystem, sink: SinkHandle) -> Self {
        let audio_renderer = Arc::new(Mutex::new(AudioRenderer::new(system.clone(), sink)));
        let opus_decoder = Arc::new(Mutex::new(OpusDecoder::new(system)));
        {
            let decoder = opus_decoder.lock();
            decoder.send(Direction::Dsp, Message::Start);
            let msg = decoder.receive(Direction::Host);
            if msg != Message::StartOK {
                log::error!("OpusDecoder failed to initialize.");
            }
        }
        Self {
            audio_renderer,
            opus_decoder,
        }
    }

    pub fn audio_renderer(&self) -> AudioRendererHandle {
        self.audio_renderer.clone()
    }

    pub fn opus_decoder(&self) -> OpusDecoderHandle {
        self.opus_decoder.clone()
    }
}

impl Drop for ADSP {
    fn drop(&mut self) {
        self.audio_renderer.lock().stop();
        self.opus_decoder.lock().shutdown();
    }
}
