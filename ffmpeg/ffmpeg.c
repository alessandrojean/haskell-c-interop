#include <stdio.h>
#include <libavformat/avformat.h>
#include <libavutil/dict.h>
#include <libavcodec/codec.h>
#include <libavcodec/codec_desc.h>
#include "ffmpeg.h"

void create_file_dict(AVDictionary ** dict, AVFormatContext * format_context);
void create_format_dict(AVDictionary ** dict, const AVInputFormat * iformat);
void create_streams_dict(AVDictionary ** dicts, AVFormatContext * format_context);
char * duration_to_string(int64_t duration, AVRational * time_base);

FfmpegInput * load_input(const char * file_name) {
  AVFormatContext * format_context = NULL;

  if (avformat_open_input(&format_context, file_name, NULL, NULL)) {
    return NULL;
  }

  if (avformat_find_stream_info(format_context, NULL) < 0) {
    av_log(NULL, AV_LOG_ERROR, "Cannot find stream information\n");
    return NULL;
  }

  FfmpegInput * input = malloc(sizeof (FfmpegInput));

  if (input == NULL) {
    fprintf(stderr, "Failed to allocate necessary bytes for input\n");
    return NULL;
  }

  AVDictionary * metadata = NULL;
  av_dict_copy(&metadata, format_context->metadata, AV_DICT_DONT_OVERWRITE);
  create_file_dict(&metadata, format_context);  

  AVDictionary * format_metadata = NULL;
  create_format_dict(&format_metadata, format_context->iformat);

  AVDictionary ** streams_metadata = calloc(format_context->nb_streams, sizeof(AVDictionary *));
  create_streams_dict(streams_metadata, format_context);

  input->context = format_context;
  input->metadata = metadata;
  input->format_metadata = format_metadata;
  input->nb_streams = format_context->nb_streams;
  input->streams_metadata = streams_metadata;  

  return input;
}

void create_file_dict(AVDictionary ** dict, AVFormatContext * format_context) {
  char * duration_fmt = duration_to_string(format_context->duration, &AV_TIME_BASE_Q);
  av_dict_set(dict, "duration", duration_fmt, AV_DICT_DONT_OVERWRITE);

  char * bit_rate_fmt = NULL;
  asprintf(&bit_rate_fmt, "%ld bit/s", format_context->bit_rate);
  av_dict_set(dict, "bit_rate", bit_rate_fmt, AV_DICT_DONT_OVERWRITE);

  char * streams_nb = NULL;
  asprintf(&streams_nb, "%d", format_context->nb_streams);
  av_dict_set(dict, "streams_nb", streams_nb, AV_DICT_DONT_OVERWRITE);
}

void create_format_dict(AVDictionary ** dict, const AVInputFormat * iformat) {
  av_dict_set(dict, "name", iformat->name, AV_DICT_DONT_OVERWRITE);
  av_dict_set(dict, "long_name", iformat->long_name, AV_DICT_DONT_OVERWRITE);
  av_dict_set(dict, "extensions", iformat->extensions, AV_DICT_DONT_OVERWRITE);
  av_dict_set(dict, "mime_type", iformat->mime_type, AV_DICT_DONT_OVERWRITE);
}

void create_streams_dict(AVDictionary ** dicts, AVFormatContext * format_context) {
  for (int i = 0; i < format_context->nb_streams; i++) {
    AVStream * curr_stream = format_context->streams[i];
    AVCodecParameters * local_codec_parameters = curr_stream->codecpar;

    av_dict_copy(&dicts[i], curr_stream->metadata, AV_DICT_DONT_OVERWRITE);

    char * time_base = NULL;
    asprintf(&time_base, "%d/%d", curr_stream->time_base.num, curr_stream->time_base.den);
    av_dict_set(&dicts[i], "time_base", time_base, AV_DICT_DONT_OVERWRITE);

    char * frame_rate = NULL;
    asprintf(&frame_rate, "%d/%d", curr_stream->r_frame_rate.num, curr_stream->r_frame_rate.den);
    av_dict_set(&dicts[i], "frame_rate", frame_rate, AV_DICT_DONT_OVERWRITE);

    char * start_time =  duration_to_string(curr_stream->start_time, &curr_stream->time_base);
    av_dict_set(&dicts[i], "start_time", start_time, AV_DICT_DONT_OVERWRITE);

    char * duration = duration_to_string(curr_stream->duration, &curr_stream->time_base);
    av_dict_set(&dicts[i], "duration", duration, AV_DICT_DONT_OVERWRITE);

    const AVCodec * local_codec = avcodec_find_decoder(local_codec_parameters->codec_id);

    if (local_codec == NULL) {
      continue;
    }

    const AVCodecDescriptor * codec_descriptor = avcodec_descriptor_get(curr_stream->codecpar->codec_id);

    if (codec_descriptor != NULL) {
      av_dict_set(&dicts[i], "codec_name", codec_descriptor->name, AV_DICT_DONT_OVERWRITE);
      av_dict_set(&dicts[i], "codec_long_name", codec_descriptor->long_name, AV_DICT_DONT_OVERWRITE);
    }

    av_dict_set(&dicts[i], "codec_tag_string", av_fourcc2str(curr_stream->codecpar->codec_tag), AV_DICT_DONT_OVERWRITE);

    if (local_codec_parameters->codec_type == AVMEDIA_TYPE_VIDEO) {
      av_dict_set(&dicts[i], "type", "video", AV_DICT_DONT_OVERWRITE);

      char * resolution = NULL;
      asprintf(&resolution, "%d x %d", local_codec_parameters->width, local_codec_parameters->height);
      av_dict_set(&dicts[i], "resolution", resolution, AV_DICT_DONT_OVERWRITE);
    } else if (local_codec_parameters->codec_type == AVMEDIA_TYPE_AUDIO) {
      av_dict_set(&dicts[i], "type", "audio", AV_DICT_DONT_OVERWRITE);

      char * channels = NULL;
      asprintf(&channels, "%d", local_codec_parameters->ch_layout.nb_channels);
      av_dict_set(&dicts[i], "channels", channels, AV_DICT_DONT_OVERWRITE);

      char * sample_rate = NULL;
      asprintf(&sample_rate, "%d Hz", local_codec_parameters->sample_rate);
      av_dict_set(&dicts[i], "sample_rate", sample_rate, AV_DICT_DONT_OVERWRITE);
    }
  }
}

void free_input(FfmpegInput * input) {
  avformat_free_context(input->context);
  av_dict_free(&input->format_metadata);
  av_dict_free(&input->metadata);

  for (int i = 0; i < input->nb_streams; i++) {
    av_dict_free(&input->streams_metadata[i]);
  }

  free(input);
}

char * duration_to_string(int64_t duration, AVRational * time_base) {
  double secs = duration * av_q2d(*time_base);
  int mins = (int) secs / 60;
  secs = secs - mins * 60;
  int hours = mins / 60;
  mins %= 60;

  char * dr_string = NULL;
  asprintf(&dr_string, "%d:%02d:%09.6f", hours, mins, secs);

  return dr_string;
}
