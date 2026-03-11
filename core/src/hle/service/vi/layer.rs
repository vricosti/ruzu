// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/layer.h

#[derive(Debug, Clone, Default)]
pub struct Layer {
    id: u64,
    owner_aruid: u64,
    display_id: u64, // Using id instead of pointer to Display
    consumer_binder_id: i32,
    producer_binder_id: i32,
    is_initialized: bool,
    is_open: bool,
}

impl Layer {
    pub fn initialize(
        &mut self,
        id: u64,
        owner_aruid: u64,
        display_id: u64,
        consumer_binder_id: i32,
        producer_binder_id: i32,
    ) {
        self.id = id;
        self.owner_aruid = owner_aruid;
        self.display_id = display_id;
        self.consumer_binder_id = consumer_binder_id;
        self.producer_binder_id = producer_binder_id;
        self.is_initialized = true;
    }

    pub fn finalize(&mut self) {
        *self = Self::default();
    }

    pub fn open(&mut self) {
        self.is_open = true;
    }

    pub fn close(&mut self) {
        self.is_open = false;
    }

    pub fn get_id(&self) -> u64 {
        self.id
    }

    pub fn get_owner_aruid(&self) -> u64 {
        self.owner_aruid
    }

    pub fn get_display_id(&self) -> u64 {
        self.display_id
    }

    pub fn get_consumer_binder_id(&self) -> i32 {
        self.consumer_binder_id
    }

    pub fn get_producer_binder_id(&self) -> i32 {
        self.producer_binder_id
    }

    pub fn is_initialized(&self) -> bool {
        self.is_initialized
    }

    pub fn is_open(&self) -> bool {
        self.is_open
    }
}
