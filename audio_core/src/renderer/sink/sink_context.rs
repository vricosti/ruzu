use crate::renderer::sink::SinkInfoBase;

#[derive(Debug, Clone, Default)]
pub struct SinkContext {
    sink_infos: Vec<SinkInfoBase>,
    sink_count: u32,
}

impl SinkContext {
    pub fn initialize(&mut self, sink_infos: Vec<SinkInfoBase>, sink_count: u32) {
        self.sink_infos = sink_infos;
        self.sink_count = sink_count;
    }

    pub fn get_info(&mut self, index: u32) -> Option<&mut SinkInfoBase> {
        self.sink_infos.get_mut(index as usize)
    }

    pub fn get_count(&self) -> u32 {
        self.sink_count
    }

    pub fn infos(&self) -> &[SinkInfoBase] {
        &self.sink_infos
    }

    pub fn infos_mut(&mut self) -> &mut [SinkInfoBase] {
        &mut self.sink_infos
    }
}
