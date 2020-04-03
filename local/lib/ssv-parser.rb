module SSV
  def parse(source)
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
end
