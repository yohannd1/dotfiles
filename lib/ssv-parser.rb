module SSV
  def tokenize(source)
    pos = 0
    queue = []
    tokens = []
    context = [:base]

    if source[pos] == "#"
      return tokens
    end

    loop do
      go_next_char = false
      ch = source[pos]

      case context.last
      when :base
        case ch
        when nil # End of string
        when ' '
          go_next_char = true

        when '"'
          context.push :str_quote
          go_next_char = true

        else
          context.push :str_unquote
        end

      when :str_unquote
        case ch
        when nil
          tokens.push queue.join
          queue.clear
          context.pop

        when ' '
          tokens.push queue.join
          queue.clear
          context.pop
          go_next_char = true

        when '"'
          context.push :str_quote
          go_next_char = true

        else
          queue.push ch
          go_next_char = true
        end

      when :str_quote
        case ch
        when nil
          return nil

        when '"'
          tokens.push queue.join
          queue.clear
          context.pop
          go_next_char = true

          # TODO: handle escape codes

        else
          queue.push ch
          go_next_char = true
        end

      else
        return nil
      end

      break if pos >= source.length # Use this before doing the main part
      pos += go_next_char ? 1 : 0
    end

    tokens
  end

  # options =>
  # prohibit_comments: bool

  def tokenize_lines(lines, allow_raise = false)
    lines_list = lines.split("\n")

    lines_list.each_with_index.map{ |contents, index|
      tokens = tokenize(contents)
      if tokens == nil
        if allow_raise
          raise "Could not tokenize line ##{index}: #{contents.inspect}"
        else
          nil
        end
      end
      tokens
    }
  end
end
