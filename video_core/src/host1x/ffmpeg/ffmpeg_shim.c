// SPDX-FileCopyrightText: Copyright 2026 ruzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include <stdint.h>
#include <stdlib.h>
#include <errno.h>

#include <libavcodec/avcodec.h>
#include <libavcodec/codec.h>
#include <libavutil/avutil.h>
#include <libavutil/error.h>
#include <libavutil/frame.h>
#include <libavutil/hwcontext.h>
#include <libavutil/opt.h>

typedef struct RuzuFfmpegDecoder {
    const AVCodec* decoder;
    AVCodecContext* context;
    AVFrame* temp_frame;
    int decode_order;
    int got_frame;
    int last_error;
    int h264_software_packet_decode;
} RuzuFfmpegDecoder;

typedef struct RuzuFfmpegHardwareContext {
    AVBufferRef* gpu_decoder;
} RuzuFfmpegHardwareContext;

static const enum AVPixelFormat RUZU_PREFERRED_GPU_FORMAT = AV_PIX_FMT_NV12;
static const enum AVPixelFormat RUZU_PREFERRED_CPU_FORMAT = AV_PIX_FMT_YUV420P;

static enum AVPixelFormat ruzu_get_gpu_format(AVCodecContext* codec_context,
                                              const enum AVPixelFormat* pix_fmts) {
    for (const enum AVPixelFormat* p = pix_fmts; *p != AV_PIX_FMT_NONE; ++p) {
        if (*p == codec_context->pix_fmt) {
            return codec_context->pix_fmt;
        }
    }

    av_buffer_unref(&codec_context->hw_device_ctx);
    codec_context->pix_fmt = RUZU_PREFERRED_CPU_FORMAT;
    return codec_context->pix_fmt;
}

static const enum AVHWDeviceType RUZU_PREFERRED_GPU_DECODERS[] = {
    AV_HWDEVICE_TYPE_CUDA,
#ifdef _WIN32
    AV_HWDEVICE_TYPE_D3D11VA,
    AV_HWDEVICE_TYPE_DXVA2,
#elif defined(__unix__)
    AV_HWDEVICE_TYPE_VAAPI,
    AV_HWDEVICE_TYPE_VDPAU,
#endif
    AV_HWDEVICE_TYPE_VULKAN,
};

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

static const AVCodec* ruzu_find_decoder(uint64_t codec) {
    const enum AVCodecID codec_id = ruzu_codec_id(codec);
    if (codec_id == AV_CODEC_ID_NONE) {
        return NULL;
    }
    return avcodec_find_decoder(codec_id);
}

RuzuFfmpegDecoder* ruzu_ffmpeg_decoder_create(uint64_t codec) {
    const AVCodec* decoder = ruzu_find_decoder(codec);
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

    RuzuFfmpegDecoder* wrapper = calloc(1, sizeof(*wrapper));
    if (wrapper == NULL) {
        avcodec_free_context(&context);
        return NULL;
    }
    wrapper->decoder = decoder;
    wrapper->context = context;
    return wrapper;
}

int ruzu_ffmpeg_decoder_open(RuzuFfmpegDecoder* decoder) {
    if (decoder == NULL || decoder->context == NULL || decoder->decoder == NULL) {
        return -1;
    }

    const int ret = avcodec_open2(decoder->context, decoder->decoder, NULL);
    decoder->last_error = ret;
    return ret;
}

RuzuFfmpegHardwareContext* ruzu_ffmpeg_hardware_context_create(void) {
    return calloc(1, sizeof(RuzuFfmpegHardwareContext));
}

void ruzu_ffmpeg_hardware_context_destroy(RuzuFfmpegHardwareContext* hardware) {
    if (hardware == NULL) {
        return;
    }
    av_buffer_unref(&hardware->gpu_decoder);
    free(hardware);
}

int ruzu_ffmpeg_decoder_supports_decoding_on_device(uint64_t codec, int device_type, int* out_pix_fmt) {
    const AVCodec* decoder = ruzu_find_decoder(codec);
    if (decoder == NULL) {
        return 0;
    }

    for (int i = 0;; i++) {
        const AVCodecHWConfig* config = avcodec_get_hw_config(decoder, i);
        if (config == NULL) {
            return 0;
        }
        if ((config->methods & AV_CODEC_HW_CONFIG_METHOD_HW_DEVICE_CTX) != 0 &&
            config->device_type == (enum AVHWDeviceType)device_type) {
            if (out_pix_fmt != NULL) {
                *out_pix_fmt = config->pix_fmt;
            }
            return 1;
        }
    }
}

uintptr_t ruzu_ffmpeg_supported_device_types(int* out, uintptr_t out_capacity) {
    uintptr_t count = 0;
    enum AVHWDeviceType current_device_type = AV_HWDEVICE_TYPE_NONE;

    while (1) {
        current_device_type = av_hwdevice_iterate_types(current_device_type);
        if (current_device_type == AV_HWDEVICE_TYPE_NONE) {
            return count;
        }

        if (out != NULL && count < out_capacity) {
            out[count] = current_device_type;
        }
        count++;
    }
}

int ruzu_ffmpeg_hardware_initialize_for_decoder(RuzuFfmpegHardwareContext* hardware,
                                                RuzuFfmpegDecoder* decoder_wrapper,
                                                uint64_t codec) {
    if (hardware == NULL || decoder_wrapper == NULL || decoder_wrapper->context == NULL) {
        return 0;
    }

    const AVCodec* decoder = ruzu_find_decoder(codec);
    if (decoder == NULL) {
        return 0;
    }

    const uintptr_t preferred_count =
        sizeof(RUZU_PREFERRED_GPU_DECODERS) / sizeof(RUZU_PREFERRED_GPU_DECODERS[0]);
    for (uintptr_t i = 0; i < preferred_count; i++) {
        const enum AVHWDeviceType type = RUZU_PREFERRED_GPU_DECODERS[i];
        AVBufferRef* candidate = NULL;
        if (av_hwdevice_ctx_create(&candidate, type, NULL, NULL, 0) < 0) {
            continue;
        }

        enum AVPixelFormat hw_pix_fmt = AV_PIX_FMT_NONE;
        for (int config_index = 0;; config_index++) {
            const AVCodecHWConfig* config = avcodec_get_hw_config(decoder, config_index);
            if (config == NULL) {
                break;
            }
            if ((config->methods & AV_CODEC_HW_CONFIG_METHOD_HW_DEVICE_CTX) != 0 &&
                config->device_type == type) {
                hw_pix_fmt = config->pix_fmt;
                break;
            }
        }

        if (hw_pix_fmt == AV_PIX_FMT_NONE) {
            av_buffer_unref(&candidate);
            continue;
        }

        av_buffer_unref(&hardware->gpu_decoder);
        hardware->gpu_decoder = candidate;
        av_buffer_unref(&decoder_wrapper->context->hw_device_ctx);
        decoder_wrapper->context->hw_device_ctx = av_buffer_ref(hardware->gpu_decoder);
        if (decoder_wrapper->context->hw_device_ctx == NULL) {
            return 0;
        }
        decoder_wrapper->context->get_format = ruzu_get_gpu_format;
        decoder_wrapper->context->pix_fmt = hw_pix_fmt;
        return 1;
    }

    return 0;
}

void ruzu_ffmpeg_decoder_destroy(RuzuFfmpegDecoder* decoder) {
    if (decoder == NULL) {
        return;
    }
    av_frame_free(&decoder->temp_frame);
    avcodec_free_context(&decoder->context);
    free(decoder);
}

int ruzu_ffmpeg_decoder_send_packet(RuzuFfmpegDecoder* decoder, const uint8_t* data, uintptr_t size) {
    if (decoder == NULL || decoder->context == NULL) {
        return -1;
    }

    av_frame_free(&decoder->temp_frame);
    decoder->temp_frame = av_frame_alloc();
    decoder->got_frame = 0;
    if (decoder->temp_frame == NULL) {
        decoder->last_error = AVERROR(ENOMEM);
        return decoder->last_error;
    }

#ifndef ANDROID
    if (decoder->context->hw_device_ctx == NULL && decoder->context->codec_id == AV_CODEC_ID_H264) {
        decoder->decode_order = 1;
        const int ret = avcodec_send_frame(decoder->context, decoder->temp_frame);
        if (ret != AVERROR(EINVAL)) {
            decoder->last_error = ret;
            return ret;
        }
        decoder->h264_software_packet_decode = 1;
    }
#endif

    AVPacket* packet = av_packet_alloc();
    if (packet == NULL) {
        decoder->last_error = AVERROR(ENOMEM);
        return -1;
    }
    packet->data = (uint8_t*)data;
    packet->size = (int)size;

    const int ret = avcodec_send_packet(decoder->context, packet);
    decoder->last_error = ret;
    av_packet_free(&packet);
    return ret;
}

int ruzu_ffmpeg_decoder_last_error(const RuzuFfmpegDecoder* decoder) {
    if (decoder == NULL) {
        return -1;
    }
    return decoder->last_error;
}

void ruzu_ffmpeg_error_string(int errnum, char* out, uintptr_t out_size) {
    if (out == NULL || out_size == 0) {
        return;
    }
    av_make_error_string(out, out_size, errnum);
}

AVFrame* ruzu_ffmpeg_decoder_receive_frame(RuzuFfmpegDecoder* decoder) {
    if (decoder == NULL || decoder->context == NULL) {
        return NULL;
    }

    AVFrame* frame = av_frame_alloc();
    if (frame == NULL) {
        return NULL;
    }

    const int ret = avcodec_receive_frame(decoder->context, frame);
    decoder->last_error = ret;
    if (ret < 0) {
        av_frame_free(&frame);
        return NULL;
    }

    return frame;
}

AVFrame* ruzu_ffmpeg_decoder_receive_frame_with_hw_transfer(RuzuFfmpegDecoder* decoder) {
    if (decoder == NULL || decoder->context == NULL) {
        return NULL;
    }

#ifndef ANDROID
    if (decoder->context->hw_device_ctx == NULL && decoder->context->codec_id == AV_CODEC_ID_H264 &&
        !decoder->h264_software_packet_decode) {
        decoder->decode_order = 1;
        int ret = 0;

        if (decoder->got_frame == 0) {
            AVPacket* packet = av_packet_alloc();
            if (packet == NULL) {
                decoder->last_error = AVERROR(ENOMEM);
                return NULL;
            }
            packet->data = NULL;
            packet->size = 0;
            ret = avcodec_receive_packet(decoder->context, packet);
            av_packet_free(&packet);
            decoder->context->has_b_frames = 0;
        }

        decoder->last_error = ret;
        if (decoder->got_frame == 0 || ret < 0) {
            return NULL;
        }

        AVFrame* output = decoder->temp_frame;
        decoder->temp_frame = NULL;
        return output;
    }
#endif

    if (decoder->context->hw_device_ctx == NULL) {
        return ruzu_ffmpeg_decoder_receive_frame(decoder);
    }

    AVFrame* intermediate = av_frame_alloc();
    AVFrame* output = av_frame_alloc();
    if (intermediate == NULL || output == NULL) {
        av_frame_free(&intermediate);
        av_frame_free(&output);
        return NULL;
    }

    int ret = avcodec_receive_frame(decoder->context, intermediate);
    decoder->last_error = ret;
    if (ret < 0) {
        av_frame_free(&intermediate);
        av_frame_free(&output);
        return NULL;
    }

    output->format = RUZU_PREFERRED_GPU_FORMAT;
    ret = av_hwframe_transfer_data(output, intermediate, 0);
    decoder->last_error = ret;
    av_frame_free(&intermediate);
    if (ret < 0) {
        av_frame_free(&output);
        return NULL;
    }

    return output;
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
