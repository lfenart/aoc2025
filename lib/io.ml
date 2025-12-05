let read_file filename =
  In_channel.with_open_text filename In_channel.input_lines
