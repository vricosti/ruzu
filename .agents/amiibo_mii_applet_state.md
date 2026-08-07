# Amiibo and Mii system applet state

## Interrupted slice

Make the frontend-initiated Cabinet `Set Nickname and Owner` action and the
Mii Editor action behave like upstream `GMainWindow::OnCabinet` and
`GMainWindow::OnMiiEdit`.

## Reproduction

Activating `app.load_mii_edit` boots firmware program
`0100000000001009`, then the guest requests `nfc:sys`. Ruzu currently
registers `nfc:am`, `nfc:mf:u`, `nfc:user`, and `nfc:sys` as
`GenericStubService`; command 3 receives a malformed success response and the
guest raises fatal result `2010-0212`. The GPU subsequently panics in
`emit_spirv_special.rs` because geometry streams are not implemented.

## Missing prerequisites

1. Completed: port the concrete NFC service entry points and interfaces from
   `zuyu/src/core/hle/service/nfc/nfc.{h,cpp}` into
   `core/src/hle/service/nfc/nfc.rs`, using the existing
   `nfc_interface.rs` implementation for command behavior.
2. Completed: verify the NFC port against upstream, add focused tests, and update
   `DIFF.md`.
3. In progress: the Mii Editor reached the SPIR-V geometry-stream panic because
   the Vulkan rasterizer omitted upstream's
   `AreTransformFeedbackGeometryStreamsSupported()` value when constructing the
   shader profile. Port that capability propagation, then resume both applets.

## Current frontend state

`ruzu/src/main_window.rs` already resolves the firmware NCA, sets the applet
ID and Cabinet mode, and supplies frontend-initiated library-applet boot
parameters matching upstream. No frontend change has been made for this slice.
