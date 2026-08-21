// SPDX-License-Identifier: GPL-3.0-or-later

fn main() {
    println!("cargo:rerun-if-changed=../../dist/ruzu.ico");
    println!("cargo:rerun-if-changed=../../dist/ruzu.manifest");
    println!("cargo:rerun-if-changed=../../dist/ruzu.rc");

    if std::env::var_os("CARGO_CFG_WINDOWS").is_some() {
        embed_resource::compile("../../dist/ruzu.rc", embed_resource::NONE)
            .manifest_required()
            .expect("failed to embed the Ruzu Windows icon and manifest");
    }
}
