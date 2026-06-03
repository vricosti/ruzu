// SPDX-FileCopyrightText: Copyright 2026 ruzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include <stdint.h>
#include <stdlib.h>

#include <libavcodec/avcodec.h>
#include <libavutil/avutil.h>
#include <libavutil/error.h>
#include <libavutil/frame.h>
#include <libavutil/opt.h>

typedef struct RuzuFfmpegDecoder {
    AVCodecContext* context;
    int decode_order;
} RuzuFfmpegDecoder;

static enum AVCodecID ruzu_codec_id(uint64_t codec) {
    switch (codec) {
    case 0x3:
        return AV_CODEC_ID_H264;
    case 0x5:
        return AV_CODEC_ID_VP8;
    case 0x9:
        return AV_CODEC_ID_VP9;
    default:
        return AV_CODEC_ID_NONE;
    }
}

RuzuFfmpegDecoder* ruzu_ffmpeg_decoder_create(uint64_t codec) {
    const enum AVCodecID codec_id = ruzu_codec_id(codec);
    if (codec_id == AV_CODEC_ID_NONE) {
        return NULL;
    }

    const AVCodec* decoder = avcodec_find_decoder(codec_id);
    if (decoder == NULL) {
        return NULL;
    }

    AVCodecContext* context = avcodec_alloc_context3(decoder);
    if (context == NULL) {
        return NULL;
    }

    if (context->priv_data != NULL) {
        av_opt_set(context->priv_data, "tune", "zerolatency", 0);
    }
    context->thread_count = 0;
    context->thread_type &= ~FF_THREAD_FRAME;

    if (avcodec_open2(context, decoder, NULL) < 0) {
        avcodec_free_context(&context);
        return NULL;
    }

    RuzuFfmpegDecoder* wrapper = calloc(1, sizeof(*wrapper));
    if (wrapper == NULL) {
        avcodec_free_context(&context);
        return NULL;
    }
    wrapper->context = context;
    return wrapper;
}

void ruzu_ffmpeg_decoder_destroy(RuzuFfmpegDecoder* decoder) {
    if (decoder == NULL) {
        return;
    }
    avcodec_free_context(&decoder->context);
    free(decoder);
}

int ruzu_ffmpeg_decoder_send_packet(RuzuFfmpegDecoder* decoder, const uint8_t* data, uintptr_t size) {
    if (decoder == NULL || decoder->context == NULL) {
        return -1;
    }

    AVPacket* packet = av_packet_alloc();
    if (packet == NULL) {
        return -1;
    }
    packet->data = (uint8_t*)data;
    packet->size = (int)size;

    const int ret = avcodec_send_packet(decoder->context, packet);
    av_packet_free(&packet);
    return ret;
}

AVFrame* ruzu_ffmpeg_decoder_receive_frame(RuzuFfmpegDecoder* decoder) {
    if (decoder == NULL || decoder->context == NULL) {
        return NULL;
    }

    AVFrame* frame = av_frame_alloc();
    if (frame == NULL) {
        return NULL;
    }

    if (avcodec_receive_frame(decoder->context, frame) < 0) {
        av_frame_free(&frame);
        return NULL;
    }

    return frame;
}

void ruzu_ffmpeg_frame_destroy(AVFrame* frame) {
    av_frame_free(&frame);
}

int ruzu_ffmpeg_frame_width(const AVFrame* frame) {
    return frame != NULL ? frame->width : 0;
}

int ruzu_ffmpeg_frame_height(const AVFrame* frame) {
    return frame != NULL ? frame->height : 0;
}

int ruzu_ffmpeg_frame_format(const AVFrame* frame) {
    return frame != NULL ? frame->format : -1;
}

int ruzu_ffmpeg_frame_stride(const AVFrame* frame, int plane) {
    if (frame == NULL || plane < 0 || plane >= AV_NUM_DATA_POINTERS) {
        return 0;
    }
    return frame->linesize[plane];
}

const uint8_t* ruzu_ffmpeg_frame_plane(const AVFrame* frame, int plane) {
    if (frame == NULL || plane < 0 || plane >= AV_NUM_DATA_POINTERS) {
        return NULL;
    }
    return frame->data[plane];
}

int ruzu_ffmpeg_frame_interlaced(const AVFrame* frame) {
    if (frame == NULL) {
        return 0;
    }
#if LIBAVUTIL_VERSION_MAJOR >= 59
    return (frame->flags & AV_FRAME_FLAG_INTERLACED) != 0;
#else
    return frame->interlaced_frame != 0;
#endif
}
