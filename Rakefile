# frozen_string_literal: true

require 'forwardable'
require 'json'
require 'logger'
require 'pathname'
require 'rake'
require 'set'
require 'shellwords'
require 'time'

module Squared
  VERSION = '0.6.9'

  module Common
    PATH = {}
    ARG = {
      PIPE: 1,
      OUT: nil,
      FAIL: false,
      HOME: nil,
      COMMON: true,
      VERBOSE: nil,
      BANNER: true,
      CHOICE: 25,
      QUOTE: "'",
      SPACE: ' => ',
      GRAPH: %w[| - | \\ -].freeze,
      BORDER: %w[| - - - - - | | - -].freeze,
      VIEW: 'view',
      BACKTRACE: $DEBUG || !$VERBOSE.nil?,
      LEVEL: ENV.fetch('LOG_LEVEL', 0).to_i,
      COLOR: ENV.fetch('NO_COLOR', '').empty?
    }
    VAR = {
      project: {},
      colors: {
        black: '30',
        red: '31',
        green: '32',
        yellow: '33',
        blue: '34',
        magenta: '35',
        cyan: '36',
        white: '37',
        black!: '40',
        red!: '41',
        green!: '42',
        yellow!: '43',
        blue!: '44',
        magenta!: '45',
        cyan!: '46',
        white!: '47'
      },
      theme: {
        workspace: {
          header: [:bold],
          active: [:bold],
          inline: [:bold],
          subject: [:bold],
          border: nil,
          warn: %i[white red!],
          caution: %i[black yellow!],
          current: nil,
          extra: nil,
          major: [:bold]
        },
        project: {},
        viewer: {
          banner: [:bold],
          border: [:blue],
          key: nil,
          value: [:green],
          string: [:yellow],
          hash: %i[green black!],
          array: %i[blue black!],
          number: [:magenta],
          boolean: [:magenta],
          undefined: %i[red italic]
        },
        logger: {
          unknown: [:cyan],
          fatal: %i[white bold red!],
          error: %i[red bold],
          warn: %i[yellow bold],
          info: [:blue],
          debug: [:green]
        }
      }
    }.compare_by_identity
    private_constant :VAR

    private

    def __get__(key)
      VAR[key.is_a?(::String) ? key.to_sym : key]
    end

    def __set__(key, val)
      return if VAR.frozen?

      VAR[key.is_a?(::String) ? key.to_sym : key] = val
    end

    def __freeze__
      PATH.freeze
      ARG.freeze
      VAR.each_value(&:freeze)
      VAR[:theme].each_value { |val| val.freeze.each_value(&:freeze) }
      VAR.freeze
    end

    module Format
      include Common

      AIX_TERM = {
        bright_black: '90',
        bright_red: '91',
        bright_green: '92',
        bright_yellow: '93',
        bright_blue: '94',
        bright_magenta: '95',
        bright_cyan: '96',
        bright_white: '97',
        bright_black!: '100',
        bright_red!: '101',
        bright_green!: '102',
        bright_yellow!: '103',
        bright_blue!: '104',
        bright_magenta!: '105',
        bright_cyan!: '106',
        bright_white!: '107'
      }.freeze
      BOX_GRAPH = %w[│ ─ ├ └ ┬].freeze
      BOX_BORDER = %w[│ ─ ┌ ┐ ┘ └ ├ ┤ ┬ ┴].tap do |val|
        if ENV['TERM']&.end_with?('256color')
          val.slice!(2, 4)
          val.insert(2, '╭', '╮', '╯', '╰')
        end
        val.freeze
      end
      TEXT_STYLE = [:bold, :dim, :italic, :underline, :blinking, nil, :inverse, :hidden, :strikethrough].freeze
      private_constant :AIX_TERM, :BOX_GRAPH, :BOX_BORDER, :TEXT_STYLE

      String.define_method(:stripstyle) { gsub(/\x1B\[(?:\d+;?)+m/, '') }
      String.define_method(:stripext) { File.basename(self, '.*') }
      String.define_method(:subhint) { |s| s.nil? || (s.is_a?(::String) && s.empty?) ? self : "#{self} (#{s})" }

      def enable_aixterm
        unless (colors = __get__(:colors)).frozen?
          colors.update(AIX_TERM)
        end
        self
      end

      def enable_drawing
        ARG[:GRAPH] = BOX_GRAPH
        ARG[:BORDER] = BOX_BORDER
        self
      end

      private

      def sub_style(val, *args, styles: nil, pat: nil, index: 1)
        return val unless ARG[:COLOR]

        ret = if pat && index != 0
                return val unless (data = pat.match(val))

                index == -1 ? data.to_a.drop(1) : data[index]
              else
                index = 0
                val
              end
        wrap = ->(s, n) { "\x1B[#{n.join(';')}m#{s}\x1B[0m" }
        code = []
        args.clear if args.size == 1 && args.first.nil?
        args.concat(Array(styles)).flatten.each_with_index do |type, i|
          next unless type

          s = if index == -1
                ret[i] || (next ret[i] = '')
              else
                ret
              end
          if type.is_a?(::Numeric)
            f, b = type.to_s.split('.')
            s = wrap.call(s, ['38', '5', f]) if f[0] != '-' && f.to_i <= 255
            if b
              b = b[0, 3]
              s = wrap.call(s, ['48', '5', b]) unless b.to_i > 255
            end
          else
            t = type.to_sym
            if (c = __get__(:colors)[t] || __get__(:colors)[t.to_s.sub('bright_', '').to_sym])
              if index == -1
                s = wrap.call(s, [c])
              else
                code << c
              end
            else
              next unless (n = TEXT_STYLE.index(t))

              s = "\x1B[#{n.succ}m#{s}\x1B[#{n == 0 ? 22 : n + 21}m"
            end
          end
          if index == -1
            ret[i] = s
          else
            ret = s
          end
        end
        return ret.join if index == -1

        ret = wrap.call(ret, code) unless code.empty?
        return ret unless data

        out = +''
        data.to_a.each_with_index do |group, i|
          next if i == 0

          if i == index
            out << ret
          elsif !group.nil?
            out << group
          end
        end
        out
      end

      def sub_style!(val, *args, **kwargs)
        val.replace(sub_style(val, *args, **kwargs))
      end

      def check_style(args, empty: true)
        ret = []
        colors = __get__(:colors)
        Array(args).flatten.compact.each do |val|
          if !val.is_a?(::Numeric)
            k = val.to_sym
            ret << k if colors.key?(k) || colors.key?(k.to_s.sub('bright_', '').to_sym) || TEXT_STYLE.include?(k)
          elsif val.between?(0, 256)
            ret << val
          elsif val < 0 && (b = val.to_s.split('.')[1])
            b = b[0, 3]
            ret << "-0.#{b}".to_f unless b.to_i > 255
          end
        end
        return if ret.empty? && !empty

        ret
      end

      def apply_style(data, key, args, empty: true)
        return if data.is_a?(::Symbol) && (data = __get__(:theme)[data]).nil?

        set = ->(k, v) { data[k.to_sym] = check_style(v, empty: empty) }
        if key.is_a?(::Hash)
          key.each { |k, v| set.call(k, v || args) }
        else
          set.call(key, args)
        end
      end

      def opt_style(styles, pat = nil, index = 1)
        { styles: styles, pat: pat, index: index }
      end

      def log_sym(level)
        if level.is_a?(::Numeric)
          case level
          when Logger::DEBUG then :debug
          when Logger::INFO then :info
          when Logger::WARN then :warn
          when Logger::ERROR then :error
          when Logger::FATAL then :fatal
          else :unknown
          end
        else
          level.to_s.downcase.to_sym
        end
      end

      def log_title(level, color: ARG[:COLOR])
        theme = __get__(:theme)[:logger]
        styles = theme[level = log_sym(level)] || theme[level = :unknown]
        case (ret = +level.to_s.upcase)
        when 'WARN', 'ERROR', 'FATAL'
          ret += '!'
        end
        color ? sub_style(ret, *styles) : ret
      end

      def log_message(level, *args, subject: nil, hint: nil, append: true, pass: false, color: ARG[:COLOR])
        if args.empty?
          args.concat(Array(level))
          level = Logger::INFO
        end
        args = args.map(&:to_s)
        if level.is_a?(::Numeric)
          if append && respond_to?(:log)
            ref = log rescue nil
            ref.add(level, message(subject, *args, hint: hint, space: ', ')) if ref.is_a?(::Logger)
          end
          return false unless pass || level >= ARG[:LEVEL]
        end
        if hint.nil? ? args.size > 1 : !hint
          title = log_title(level, color: false)
          emphasize(args,
                    title: title + (subject ? " #{subject}" : ''),
                    pipe: -1,
                    sub: if color
                           opt_style(__get__(:theme)[:logger][log_sym(level)], /\A(#{Regexp.escape(title)})(.*)\z/m)
                         end)
        else
          msg = [log_title(level, color: color)]
          if subject
            msg << (color ? sub_style(subject.to_s, (@theme.is_a?(::Hash) && @theme[:subject]) || :bold) : subject)
          end
          msg << args.shift if msg.size == 1
          message(msg.join(' '), *args, hint: hint)
        end
      end

      def log_warn(*args, **kwargs)
        log_message(Logger::WARN, *args, **kwargs)
      end

      def log_console(*args, pipe: 1)
        return if args.first == false && args.size == 1

        if pipe.is_a?(::Pathname)
          begin
            File.open(pipe, 'a') do |f|
              br = File::SEPARATOR == '\\' ? "\r\n" : "\n"
              args.flatten.each { |val| f.write(val.chomp.stripstyle + br) }
            end
            return
          rescue StandardError
            pipe = 2
          end
        end
        (pipe == 2 ? $stderr : $stdout).puts(*args)
      end

      module_function

      def message(*args, hint: nil, empty: false, space: ARG[:SPACE])
        (empty ? args.reject { |val| val.nil? || (val.respond_to?(:empty?) && val.empty?) } : args)
          .join(space)
          .subhint(hint)
      end

      def emphasize(val, title: nil, footer: nil, right: false, cols: nil, sub: nil, pipe: nil,
                    border: @theme.is_a?(::Hash) && @theme[:border])
        n = 0
        max = ->(a) { n = [n, a.max_by(&:size).size].max }
        set = ->(s) { Array(s).map(&:to_s).tap { |a| max.call(a) } }
        title &&= set.call(title)
        footer &&= set.call(footer)
        if val.is_a?(::Array)
          lines = val.map(&:to_s)
        else
          lines = val.to_s.lines(chomp: true)
          lines[0] = "#{val.class}: #{lines.first}" if (err = val.is_a?(::StandardError))
        end
        return if lines.empty?

        n = (cols.is_a?(::Array) ? cols.map(&:size).max : cols) || max.call(lines)
        if $stdout.tty?
          require 'io/console'
          (n = [n, $stdout.winsize[1] - 4].min) rescue nil
        end
        b0, b1, b2, b3, b4, b5, b6, b7 = ARG[:BORDER]
        out = []
        draw = lambda do |a, b|
          ret = a + (b1 * (n + 2)) + b
          return ret unless border

          sub_style ret, border
        end
        sub = sub.is_a?(::Hash) ? [sub] : Array(sub)
        pr = lambda do |line|
          s = line.ljust(n)
          sub.each { |h| sub_style!(s, **h) }
          s = +"#{b0} #{s} #{b0}"
          if border
            [[/\A(#{Regexp.escape(b0)})(.+)\z/om], [/\A(.+)(#{Regexp.escape(b0)})\z/om, 2]].each do |args|
              sub_style!(s, **opt_style(border, *args))
            end
          end
          s
        end
        out << draw.call(b2, b3)
        if title
          out.concat(title.map! { |t| pr.call(t) })
          out << draw.call(b6, b7)
        end
        lines.each { |line| out << pr.call(line) }
        out << draw.call(b5, b4)
        if footer
          unless sub.empty? && !right
            footer.map! do |s|
              s = s.rjust(n + 4) if right
              sub.each { |h| sub_style!(s, **h) }
              s
            end
          end
          out.concat(footer)
        end
        if block_given?
          yield out
        elsif pipe
          return out if pipe == -1

          case pipe
          when 0
            $stdin
          when 2
            $stderr
          else
            pipe.respond_to?(:puts) ? pipe : $stdout
          end.puts(out)
        else
          err ? warn(out) : puts(out)
        end
      end

      def raise_error(*args, hint: nil, kind: RuntimeError, start: 0)
        kind = args.shift if args.first.is_a?(::Class) && args.first < ::Exception
        raise kind, message(*args, hint: hint, empty: true), if ARG[:BACKTRACE]
                                                               caller(start.succ)
                                                             else
                                                               caller_locations(start.succ, 1).first&.base_label
                                                             end
      end
    end

    module Prompt
      module_function

      def confirm(msg, default = nil, agree: 'Y', cancel: 'N', force: false, attempts: 3, timeout: 60)
        require 'timeout'
        if agree == 'Y' && cancel == 'N' && !msg.match?(%r{\[(?:Yn|nY|Y/n|y/N)\]})
          case default
          when 'Y'
            msg = "#{msg} [Y/n] "
          when 'N'
            msg = "#{msg} [y/N] "
          end
        end
        agree = /^#{Regexp.escape(agree)}$/i if agree.is_a?(::String)
        cancel = /^#{Regexp.escape(cancel)}$/i if cancel.is_a?(::String)
        Timeout.timeout(timeout) do
          while (ch = Readline.readline(msg))
            ch = ch.chomp
            case (ch.empty? ? default : ch)
            when agree
              return true
            when cancel
              exit 1 if force
              return false
            end
            attempts -= 1
            exit 1 unless attempts > 0
          end
        rescue Interrupt
          puts
          exit 0
        else
          exit 1 if force
          false
        end
      end

      def choice(msg, list = nil, min: 1, max: 1, multiple: false, index: false, grep: nil, border: nil, auto: true,
                 force: true, attempts: 3, timeout: 0)
        require 'timeout'
        if list
          grep &&= Array(grep).map { |val| Regexp.new(val) }
          items = []
          list.each do |val|
            next if grep&.none? { |pat| pat.match?(line) }

            items << val.to_s.chomp
            puts '%2d. %s' % [items.size, val]
          end
          max = items.size
          raise ArgumentError, 'empty selection list' if max == 0

          min = grep ? 1 : [min, max].min
          if auto
            auto.times { puts } if auto.is_a?(::Numeric)
            if border == true
              puts print_footer
            elsif border
              puts print_footer(border: border)
            end
            msg = "#{msg + (force ? ':' : '?')} [#{min}-#{max}#{if (n = multiple)
                                                                  "|,#{n.is_a?(::Numeric) ? "{#{n}}" : '*'}"
                                                                end}] "
          end
        end
        between = ->(s) { s.match?(/^\d+$/) && s.to_i.between?(min, max) }
        Timeout.timeout(timeout) do
          while (ch = Readline.readline(msg))
            unless (ch = ch.strip).empty?
              if multiple
                k = if ch == '*'
                      (min..max).to_a
                    else
                      ch.split(',').map! do |s|
                        s.strip!
                        if s =~ /^(\d+)-(\d+)$/
                          next unless between.call($1) && between.call($2)

                          i = $1.to_i
                          j = $2.to_i
                          next (i..j).to_a if i < j
                        elsif between.call(s)
                          s.to_i
                        end
                      end
                    end
                unless k.include?(nil)
                  k.flatten!
                  k.uniq!
                  k.sort!
                  unless multiple.is_a?(::Numeric) && multiple != k.size
                    return index || !items ? k : k.map! { |i| items[i.pred] }
                  end
                end
              elsif between.call(ch)
                return index || !items ? ch.to_i : items[ch.to_i.pred]
              end
            end
            attempts -= 1
            next if attempts > 0

            exit 1 if force
            break
          end
        rescue Interrupt
          puts
          exit 0
        else
          [] if multiple
        end
      end

      def readline(msg, history = false, force: nil, multiline: nil, &blk)
        multiline = if multiline && Readline.respond_to?(:readmultiline)
                      multiline.is_a?(::Enumerable) || block_given? ? multiline : [multiline.to_s]
                    end
        read = lambda do
          if !multiline
            Readline.readline(msg, history)
          elsif block_given?
            Readline.readmultiline(msg, history, &blk)
          else
            Readline.readmultiline(msg, history) do |line|
              next if line.strip.empty?

              multiline.any? { |val| line.split.last.end_with?(val.to_s) }
            end
          end
        end
        case force
        when ::TrueClass, ::FalseClass
          msg = "#{msg}%s%s " % if multiline
                                  [' ', multiline.is_a?(::Enumerable) ? "{#{multiline.to_a.join('|')}}" : multiline]
                                else
                                  [force ? ':' : '?', '']
                                end
          ret = (read.call || '').strip
          multiline.each { |val| break if ret.delete_suffix!(val.to_s) } if multiline.is_a?(::Enumerable)
          exit 1 if force && ret.empty?
          ret
        else
          read.call
        end
      end
    end

    module Shell
      QUOTE_VALUE = /\A(["'])(.*)\1\z/m.freeze
      private_constant :QUOTE_VALUE

      String.define_method(:stripquote) { sub(QUOTE_VALUE, '\2') }
      Array.define_method(:quote!) { |**kwargs| map! { |s| Shell.shell_quote(s, **kwargs) } }

      module_function

      def shell_escape(val, quote: false, option: false, force: false, double: false, override: false)
        if (r = /\A(--?)([^=\s]+)((=|\s+)(["'])?(?(5)(.*)\5|(.*)))?\z/m.match(val = val.to_s))
          if (data = r[2].match(QUOTE_VALUE))
            double = data[1] == '"'
            override = true
          elsif !r[3] || r[6]
            return val
          end
          opt = if r[7].match?(/\A["']/)
                  "#{r[7]}#{r[7][0]}"
                elsif r[7].match?(/["']\z/)
                  "#{r[7][-1]}#{r[7]}"
                else
                  return val unless r[7].match?(/\s/)

                  r[7]
                end
          r[1] + (data ? data[2] : r[2]) + r[4] + shell_quote(opt, force: force, double: double, override: override)
        elsif option && val =~ /\A(-{0,2}[^=\s-][^=\s]*)=(.+)\z/m
          return val if $2.match?(QUOTE_VALUE)

          "#{$1}=%s" % if $2.include?(' ')
                         shell_quote($2, option: false)
                       elsif Rake::Win32.windows?
                         $2
                       else
                         Shellwords.escape($2)
                       end
        elsif Rake::Win32.windows?
          quote ? shell_quote(val, force: force, double: double) : val
        elsif val.empty?
          ''
        else
          Shellwords.escape(val)
        end
      end

      def shell_quote(val, option: true, force: true, double: false, preserve: true, override: false)
        val = val.to_s
        return val if (!force && !val.include?(' ')) || val.empty?

        if option
          pat = /\A(?:-[^=\s-](?:=|\s+)?|(--)?[^=\s-][^=\s]*(?(1)(?:=|\s+)|=))(["']).+\2\z/m
          return val if val.match?(pat)
        end
        q = ->(s) { s.gsub("'\\\\''", "'") }
        if val =~ QUOTE_VALUE
          return val if $1 == '"' && Rake::Win32.windows? && val.match?(/(?:[#{File::SEPARATOR} ]|\\")/o)

          base = $2 unless preserve
        end
        if double || Rake::Win32.windows? || (ARG[:QUOTE] == '"' && !override)
          "\"#{q.call(base || val).gsub(/(?<!\\)"/, '\\"')}\""
        else
          base ? val : "'#{q.call(val).gsub("'", "'\\\\''")}'"
        end
      end

      def shell_option(flag, val = nil, sep: '=', escape: true, quote: true, force: true, double: false, merge: false,
                       override: false)
        flag = flag.to_s
        if flag =~ QUOTE_VALUE
          double = $1 == '"'
          flag = $2
          escape = false
          override = true
        end
        sep = unless flag.empty?
                if flag[0] == '-'
                  if flag[1] == '-'
                    sep
                  else
                    merge ? '' : ' '
                  end
                elsif flag.size == 1
                  pre = '-'
                  merge ? '' : ' '
                else
                  pre = '--'
                  sep
                end
              end
        "#{pre}#{flag}#{unless val.nil?
                          "#{sep}#{if escape
                                     shell_escape(val, quote: quote, double: double, override: override)
                                   elsif quote
                                     shell_quote(val, option: false, force: force, double: double, override: override)
                                   else
                                     val
                                   end}"
                        end}"
      end

      def shell_split(val, join: nil, **kwargs)
        ret = val.shellsplit.map! { |opt| shell_escape(opt, option: true, double: true, **kwargs) }
        return ret unless join

        ret.join(join.is_a?(::String) ? join : ' ')
      end

      def shell_parse(val, escape: false, force: true, **kwargs)
        a = []
        b = []
        c = []
        d = []
        e = [a, b]
        j = -1
        val.shellsplit.each_with_index do |opt, i|
          if opt == '--'
            e = [c, d]
          elsif opt =~ /\A--?[^=]+(=|\z)/
            j = $1 == '=' ? -1 : i
            e[0] << [opt]
          elsif j >= 0
            e[0][j] << opt
          else
            e[1] << shell_quote(opt, option: false, force: force)
          end
        end
        ret = [[a, b], [], [c, d]].flat_map do |e, f|
          next '--' unless e

          e.flat_map do |item|
            if item.size == 1
              fill_option(item.first)
            else
              flag = item.shift
              item.map! { |s| shell_option(flag, s, escape: escape, force: force, **kwargs) }
            end
          end.concat(f)
        end
        ret.pop if ret.last == '--'
        ret
      end

      def shell_bin(name, env: true)
        key = name.to_s.upcase
        key = File.basename(key, '.*') if Rake::Win32.windows?
        shell_quote((env && ENV["PATH_#{key}"]) || PATH[key] || PATH[key.to_sym] || name,
                    option: false, force: false, double: true)
      end

      def line_width(lines)
        ret = [lines.empty? ? 0 : lines.max_by(&:size).size, 80].max
        [ret, Rake.application.terminal_width].min
      end

      def fill_option(val, **kwargs)
        return val unless val.is_a?(::String)
        return "-#{val}" if val.match?(/\A(?:[a-z]\d*|\d)\z/i)

        shell_escape(val.start_with?('-') ? val : "--#{val}", **kwargs)
      end

      def quote_option(flag, val, **kwargs)
        shell_option(flag, val, escape: false, **kwargs)
      end

      def basic_option(flag, val, **kwargs)
        shell_option(flag, val, escape: false, force: false, **kwargs)
      end
    end

    module System
      class << self
        private

        def parse_link(val)
          case val
          when ::TrueClass, 's'
            1
          when 'r'
            2
          when 'h'
            3
          else
            raise ArgumentError, "unrecognized 'link' flag: #{val}" if val

            0
          end
        end
      end

      module_function

      def shell(*args, name: :system, **kwargs)
        if RUBY_ENGINE == 'jruby' && Rake::Win32.windows?
          e = kwargs[:exception]
          if (dir = kwargs[:chdir]) && ((pwd = Dir.pwd) != dir)
            Dir.chdir dir
            ret = Kernel.send(name, *args)
            Dir.chdir pwd
          else
            ret = Kernel.send(name, *args)
          end
        elsif RUBY_VERSION < '2.6'
          e = kwargs.delete(:exception)
          ret = Kernel.send(name, *args, **kwargs)
        else
          return Kernel.send(name, *args, **kwargs)
        end
        return ret unless e && !ret && name == :system

        raise $?.to_s
      end

      def copy_dir(src, dest, glob = ['**/*'], create: false, link: nil, preserve: nil, force: false, verbose: true,
                   pass: nil, hidden: false)
        base = Pathname.new(src)
        target = Pathname.new(dest)
        raise Errno::ENOENT, dest.cleanpath.to_s unless create || target.parent.exist?

        subdir = {}
        target.mkpath if create
        flags = hidden ? [File::FNM_DOTMATCH] : []
        if pass
          exclude = []
          Array(pass).each { |val| exclude.concat(Dir.glob(val, *flags, base: base)) }
        end
        Array(glob).each do |val|
          Dir.glob(val, *flags, base: base) do |file|
            next if exclude&.include?(file) || (entry = base + file).directory?

            dir = target.join(file).dirname
            if (data = subdir[dir.to_s])
              data << entry
            else
              dir.mkpath
              subdir[dir.to_s] = [entry]
            end
          end
        end
        count = 0
        soft = 0
        type = System.send :parse_link, link
        subdir.each do |dir, files|
          unless type == 0
            items = files.dup
            files.clear
            items.each do |file|
              if file.exist?
                if !file.symlink?
                  files << file
                elsif !force
                  next
                end
              end
              case type
              when 1
                FileUtils.ln_s(file, dir, force: force, verbose: false)
              when 2
                FileUtils.ln_s(file.relative_path_from(dir), dir, force: force, verbose: false)
              else
                FileUtils.ln(file, dir, force: force, verbose: false)
              end
              soft += 1
            end
          end
          next if files.empty?

          out = FileUtils.cp(files, dir, preserve: preserve, verbose: false)
          count += out.size
        end
        puts [target.realpath, subdir.size, soft > 0 ? "#{count}+#{soft}" : count].join(' => ') if verbose
      end

      def copy_guard(*src, dest, base: '.', create: false, link: nil, preserve: nil, force: false, verbose: true)
        src = src.compact.flatten
        dest = Pathname.new(dest).realdirpath
        base = Pathname.new(base).realpath
        dir = if dest.directory?
                true
              elsif src.size > 1
                raise Errno::ENOENT, dest.cleanpath.to_s unless create && !dest.exist?

                dest.mkpath
                true
              end
        targets = src.map! { |file| [base + file, dir ? dest + File.basename(file) : dest] }
        return if !force && (targets = targets.reject { |to| to[1].exist? }).empty?

        type = System.send :parse_link, link
        targets.each do |file, to|
          case type
          when 0
            FileUtils.cp(file, to, preserve: preserve, verbose: verbose)
          when 1
            FileUtils.ln_s(file, to, force: force, verbose: verbose)
          when 2
            FileUtils.ln_s(file.relative_path_from(dir ? to.dirname : to), to, force: force, verbose: verbose)
          else
            FileUtils.ln(file, to, force: force, verbose: verbose)
          end
        end
        nil
      end
    end

    module Utils
      module_function

      def as_a(obj, *meth, flat: nil, compact: false, &blk)
        return [] if obj.nil?

        unless obj.is_a?(::Array)
          obj = if obj.respond_to?(:to_ary)
                  obj.to_ary
                elsif obj.respond_to?(:to_a) && !obj.is_a?(::Hash) && (val = obj.to_a).is_a?(::Array)
                  val
                else
                  [obj]
                end
        end
        obj = flat.is_a?(::Numeric) ? obj.flatten(flat) : obj.flatten if flat
        obj = obj.compact if compact
        obj = obj.map(&meth.shift) until meth.empty?
        return obj unless block_given?

        obj.select(&blk)
      end

      def split_escape(val, char: ',', &blk)
        ret = val.split(/\s*(?<!\\)#{char}\s*/)
        return ret unless block_given?

        ret.each(&blk)
      end

      def split_option(val)
        val = val.strip
        return [val, '', ''] unless (i = val.index('='))

        last = val[i.succ..-1].strip
        quote = ''
        if last =~ /\A(["'])(.+)\1\z/
          last = $2
          quote = $1
        end
        [val[0..i.pred], last, quote]
      end

      def task_invoke(*cmd, args: [], exception: true, warning: true)
        cmd.each { |name| Rake::Task[name].invoke(*args) }
      rescue StandardError => e
        raise if exception

        warn e if warning
      end

      def task_join(*val)
        case val.size
        when 1
          val[0].to_s
        when 2
          "#{val[0]}:#{val[1]}"
        else
          val.join(':')
        end
      end

      def task_invoked?(*args)
        Rake::Task.tasks.any? do |obj|
          obj.already_invoked && args.any? { |val| val.is_a?(::Regexp) ? obj.name.match?(val) : val == obj.name }
        end
      end

      def time_format(epoch, clock: false, pass: [])
        ss = 1000
        mm = 60 * ss
        hh = 60 * mm
        dd = 24 * hh
        hm = pass.include?('s')
        time = []
        if !clock && (d = epoch / dd) > 0
          time << "#{d}d"
          epoch -= d * dd
        end
        if (h = epoch / hh) > 0
          time << (clock ? h.to_s : "#{h}h")
          epoch -= h * hh
        end
        if (m = epoch / mm) > 0
          time << (clock ? m.to_s.rjust(2, '0') : "#{m}m")
          epoch -= m * mm
        elsif clock
          time << '00'
        end
        unless hm
          if (s = epoch / ss) > 0
            time << (clock ? s.to_s.rjust(2, '0') : "#{s}s")
            epoch -= s * ss
          elsif clock
            time << '00'
          end
        end
        if clock
          time.join(':')
        else
          time << "#{epoch}ms" unless hm || pass.include?('ms')
          time.join(' ')
        end
      end

      def time_since(val, ms: true)
        time_epoch(ms: ms) - time_epoch(Time.parse(val), ms: ms)
      end

      def time_epoch(val = Time.now, ms: true)
        val.utc.strftime(ms ? '%s%L' : '%s').to_i
      end

      def rand_s(size)
        if RUBY_VERSION >= '3.1'
          require 'random/formatter'
          Random.new.alphanumeric(size)
        else
          (0...size).map { rand(97..122).chr }.join
        end
      end

      def env(key, default = nil, suffix: nil, strict: false, equals: nil, ignore: nil, **)
        ret = env_value(key, suffix: suffix, strict: strict)
        return Array(equals).any? { |val| val.to_s == ret } unless equals.nil?

        ret.empty? || (ignore && Array(ignore).any? { |val| val.to_s == ret }) ? default : ret
      end

      def env_key(*val)
        val.join('_').gsub(/\W+/, '_').upcase
      end

      def env_value(key, default = '', suffix: nil, strict: false)
        if suffix
          if (ret = ENV["#{key + (@envname ? "_#{@envname}" : '')}_#{suffix}"])
            return ret
          elsif strict
            return default
          end
        end
        if @envname
          return ret if (ret = ENV["#{key}_#{@envname}"])
          return default if strict
        end
        ENV.fetch(key, default)
      end

      def env_bool(key, default = false, suffix: nil, strict: false, index: false)
        case key
        when ::NilClass
          default
        when ::String
          case (val = env_value(key, suffix: suffix, strict: strict))
          when ''
            default
          when '0', 'false'
            false
          else
            index && val.to_i > 0 ? val.to_i : true
          end
        else
          key
        end
      end

      def env_pipe(key, default = 1, suffix: nil, strict: false, root: nil)
        case key
        when ::String
          case (ret = env_value(key, suffix: suffix, strict: strict))
          when '0', '1', '2'
            return ret.to_i
          end
        when ::Numeric
          return key if key.between?(0, 2)
        end
        return default unless default.is_a?(::String)

        begin
          (root ? Pathname.new(root) + default : Pathname.new(default)).realdirpath
        rescue StandardError => e
          warn e
          1
        end
      end

      def env_match(key, default = nil, suffix: nil, strict: false, options: 0, timeout: nil)
        case (val = env_value(key, suffix: suffix, strict: strict))
        when ''
          default
        when '0'
          false
        when '1'
          true
        else
          Regexp.new(val, options, timeout: timeout)
        end
      end
    end
  end

  module Workspace
    module Support
      RunData = Struct.new('RunData', :run, :block)
      ChainData = Struct.new('ChainData', :action, :step, :with, :before, :after, :sync)
      BannerData = Struct.new('BannerData', :command, :order, :styles, :border)

      module Variables
        private

        def hashobj
          Hash.new { |data, key| data[key] = {} }
        end

        def hashlist
          Hash.new { |data, key| data[key] = [] }
        end

        def hashdup(data, compact: false, freeze: false, target: {}, pass: {})
          data.each do |key, val|
            next if val.nil? && compact

            target[key] = case val
                          when Hash
                            if pass.key?(val)
                              pass[val]
                            else
                              hashdup(val, compact: compact, freeze: freeze, target: pass[val] = {}, pass: pass)
                            end
                          when Enumerable
                            compact ? val.compact : val.dup
                          when Proc, Method
                            val
                          else
                            val.dup
                          end
            target[key].freeze if freeze && val.frozen?
          end
          target
        end
      end

      class << Support
        include Variables

        public(*Variables.private_instance_methods(false))
      end
    end

    class Application
      include Enumerable
      include Common::Format
      include Utils
      include Support::Variables
      include Rake::DSL

      TASK_METADATA = Rake::TaskManager.record_task_metadata
      private_constant :TASK_METADATA

      class << self
        def implement(*objs, base: false)
          return if base && objs.size > 1

          objs.each do |obj|
            next unless base || obj < impl_project

            if base
              self.impl_project = obj
              impl_series.base_set(obj)
            else
              kind_project.unshift(obj)
              impl_series.extend_set(obj)
            end
            if (args = obj.batchargs)
              impl_series.batch(*args)
            end
            if (args = obj.aliasargs)
              impl_series.alias(*args)
            end
            if (args = obj.bannerargs)
              attr_banner.merge(args)
            end
          end
        end

        def find(ref = nil, path: nil)
          if ref && (ret = kind_project.find { |proj| proj.ref == ref })
            ret
          elsif path
            kind_project.find { |proj| proj.config?(path) }
          end
        end

        def exclude(*args)
          @task_exclude.merge(args.map!(&:to_sym))
        end

        def register(app)
          @session << app
          impl_series.new(app, exclude: @task_exclude.to_a)
        end

        def load_ref(path, gem: nil)
          if gem
            unless @gemsdir
              IO.popen('bundle env').each do |val|
                next unless val =~ /^\s+Gem Home\s+(.+)$/

                @gemsdir = File.join($1, 'gems')
                break
              end
            end
            dir = if gem.match?(/-\d+(?:\.|$)/)
                    gem
                  else
                    Dir.glob("#{gem}-*", base: @gemsdir).pop
                  end
            path = File.join(@gemsdir, dir, path) if dir
          end
          @load_project.unshift(path.cleanpath.to_s) if (path = Pathname.new(path)).absolute? && path.exist?
        end

        def baseref
          impl_project.ref
        end

        def to_s
          super[/[^:]+\z/, 0]
        end

        alias series_wrap register

        attr_reader :kind_project, :load_project, :session
        attr_accessor :impl_series, :impl_project, :attr_banner
      end

      @kind_project = []
      @load_project = [File.expand_path('project', __dir__)]
      @session = []
      @task_exclude = Set.new

      attr_reader :root, :home, :main, :prefix, :theme, :series, :closed
      attr_accessor :exception, :pipe, :verbose, :warning

      def initialize(home = (ARG[:HOME] && ENV[ARG[:HOME]]) || Dir.pwd, *, main: nil, prefix: nil,
                     verbose: ARG[:VERBOSE], common: ARG[:COMMON], pipe: ARG[:PIPE], exception: ARG[:FAIL], **)
        @home = Pathname.new(home).realdirpath
        basename = @home.basename.to_s
        if main
          @main = main.to_s.freeze
          @home += @main unless @main == basename || (windows? && @main.casecmp?(basename))
        else
          @main = basename.freeze
        end
        @home.mkpath rescue nil
        @root = @home.parent
        @prefix = prefix
        @series = Application.register(self)
        @project = {}
        @kind = hashlist
        @extensions = []
        @envname = env_key(@main).freeze
        self.exception = env_bool exception
        self.pipe = $DEBUG ? 2 : env_pipe(pipe, (ARG[:OUT] && env(ARG[:OUT])) || 1, root: @home)
        self.verbose = if $VERBOSE.nil?
                         false
                       elsif verbose.nil?
                         @pipe != 0
                       else
                         env_bool(verbose, verbose.is_a?(String) ? @pipe != 0 : verbose, index: true)
                       end
        self.warning = @verbose != false
        @closed = false
        @theme = if common
                   ARG[:COLOR] = false if @pipe == 0 || @pipe.is_a?(Pathname)
                   __get__(:theme)[:workspace]
                 else
                   {}
                 end
        @chain = hashlist
        @script = {
          group: hashobj,
          ref: hashobj,
          group!: {},
          ref!: {}
        }.freeze
        @events = {
          group: hashobj,
          ref: hashobj
        }.freeze
        @pass = {
          group: hashobj,
          ref: hashobj,
          global: {},
          pattern: []
        }.freeze
        @banner = {
          group: {},
          ref: {}
        }.freeze
        initialize_session
      end

      def initialize_session
        return unless @pipe.is_a?(Pathname)

        msg = "Session started on #{Time.now} by #{@main}"
        bord = '#' * msg.size
        puts bord, msg, bord
      end

      def each(&blk)
        return to_enum(:each) unless block_given?

        @project.each_value(&blk)
      end

      def build(parallel: [], pass: nil, **kwargs)
        return self unless enabled? && !@closed

        kwargs[:parallel] = if kwargs[:pattern].is_a?(Array)
                              parallel.map(&:to_s)
                            else
                              kwargs[:pattern] = []
                              parallel.reject { |val| kwargs[:pattern] << val if val.is_a?(Regexp) }.map!(&:to_s)
                            end
        @pass[:pattern].concat(pass.map { |val| val.is_a?(Regexp) ? val : val.to_s }) if pass
        series.reset
        each do |proj|
          if proj.enabled?
            proj.populate(series.keys.dup, **kwargs)
          elsif proj.enabled?(base: false)
            proj.generate([], **kwargs)
          else
            next
          end
          series.populate(proj, **kwargs)
        end
        Application.kind_project.each { |obj| obj.populate(self, **kwargs) }
        @extensions.uniq.each { |ext| __send__(ext, **kwargs) }
        series.build(**kwargs)
        __build__(**kwargs)
        yield self if block_given?
        __chain__(**kwargs)
        @closed = true
        self
      end

      def with(*val, hide: nil, group: nil, **kwargs, &blk)
        if hide.nil? && kwargs.key?(:pass)
          pass = kwargs[:pass]
          case pass
          when true, false
            hide = pass
            kwargs.delete(:pass)
          else
            hide, pass = Array(pass).partition { |s| respond_to?(s) || s.to_s.end_with?('?') }
            if pass.empty?
              kwargs.delete(:pass)
            elsif hide.empty?
              hide = nil
            else
              kwargs[:pass] = pass
            end
          end
        end
        return self if hide == true || (hide && Array(hide).any? { |s| respond_to?(s) && __send__(s) rescue nil })

        @group = nil
        @ref = nil
        @withargs = unless kwargs.empty?
                      kwargs.delete(:parent)
                      kwargs
                    end
        val = as_a(group || kwargs[:ref], flat: true, compact: true) if val.empty?
        kind = val.first
        val = kind if val.size == 1
        case kind
        when String
          @group = val
        when Symbol
          @ref = val
        else
          raise_error ArgumentError, 'missing group or ref' if block_given?
        end
        if block_given?
          instance_eval(&blk)
          @group = nil
          @ref = nil
          @withargs = nil
        end
        self
      end

      def run(script = nil, group: @group, ref: @ref, on: nil, &blk)
        script_command :run, script, group, ref, on, &blk
      end

      def chain(task, *action, project: nil, step: 0, with: nil, before: nil, after: nil, sync: false,
                group: @group, ref: @ref)
        if project
          action.map! { |val| task_join(project.name, val) }
        elsif (target = group || ref)
          action.map! { |val| task_name(task_join(val, target)) }
        else
          action.map! { |val| task_name(val) }
          keys = @project.keys unless prefix
        end
        ns = lambda do |val|
          return if (ret = as_a(val, :to_s, flat: true)).empty?

          ret.map! do |arg|
            if arg.include?(':') || (keys && !keys.include?(arg))
              task_name(arg)
            else
              /#{Regexp.escape(arg)}:/
            end
          end
        end
        data = Struct::ChainData.new(action, step, ns.call(with), ns.call(before), ns.call(after), sync)
        @chain[task_name(task.to_s)] << data
        self
      end

      def script(script, group: @group, ref: @ref, on: nil)
        script_command :script, script, group, ref, on
      end

      def depend(script = nil, group: @group, ref: @ref, on: nil, &blk)
        script_command :depend, script, group, ref, on, &blk
      end

      def graph(script, group: @group, ref: @ref, on: nil)
        script_command :graph, as_a(script, :to_s).freeze, group, ref, on
      end

      def clean(script = nil, group: @group, ref: @ref, on: nil, &blk)
        script_command :clean, script, group, ref, on, &blk
      end

      def doc(script = nil, group: @group, ref: @ref, on: nil, &blk)
        script_command :doc, script, group, ref, on, &blk
      end

      def lint(script = nil, group: @group, ref: @ref, on: nil, &blk)
        script_command :lint, script, group, ref, on, &blk
      end

      def test(script = nil, group: @group, ref: @ref, on: nil, &blk)
        script_command :test, script, group, ref, on, &blk
      end

      def log(script, group: @group, ref: @ref)
        script_command :log, script, group, ref
      end

      def exclude(base, group: @group, ref: @ref)
        script_command :exclude, as_a(base, :to_sym).freeze, group, ref
      end

      def pass(name, group: @group, ref: @ref, &blk)
        data = if group
                 @pass[:group][group.to_s]
               elsif ref
                 @pass[:ref][ref.to_sym]
               else
                 @pass[:global]
               end
        data[name.to_sym] = blk
        self
      end

      def banner(*args, command: true, styles: nil, border: nil, group: @group, ref: @ref)
        data = Struct::BannerData.new(command, [], check_style(styles, empty: false), check_style(border))
        args.each do |meth|
          if meth.is_a?(Array)
            found = false
            meth = meth.select do |val|
              case val
              when Symbol
                found = true
                Application.attr_banner.include?(val)
              when String
                true
              else
                false
              end
            end
            if !found
              next
            elsif meth.size == 1
              meth = meth.first
            end
          elsif !Application.attr_banner.include?(meth = meth.to_sym)
            next
          end
          data.order << meth
        end
        Array(if group
                label = :group
                group
              else
                label = :ref
                ref || :_
              end).each { |val| @banner[label][val.to_sym] = data }
        self
      end

      def add(path, project = nil, **kwargs, &blk)
        kwargs = hashdup(@withargs).update(kwargs) if @withargs
        ref = kwargs.key?(:ref) ? kwargs.delete(:ref) : @ref
        kwargs[:group] = @group if @group && !kwargs.key?(:group)
        path = rootpath path
        project = (project || path.basename).to_s
        name = task_name project
        index = 0
        while @project[name]
          index += 1
          name = task_name "#{project}-#{index}"
        end
        proj = ((if !ref.is_a?(Class)
                   require_project ref
                   Application.find(ref, path: path)
                 elsif ref < Application.impl_project
                   ref
                 end) || @kind[name]&.last || Application.impl_project).new(self, path, name, **kwargs)
        proj.__send__(:index_set, size)
        @project[name] = proj
        __get__(:project)[name] = proj unless kwargs[:private]
        proj.instance_eval(&blk) if block_given?
        self
      end

      def group(path, val, override: {}, **kwargs, &blk)
        rootpath(path).children.map! do |dir|
          next unless dir.directory?

          basename = dir.basename.to_s
          [dir, basename, override[basename.to_sym]]
        end
        .each do |dir, basename, opts|
          args = kwargs.dup
          args.update(opts) if opts
          add(dir, basename, group: val, **args, &blk)
        end
        self
      end

      def compose(name, &blk)
        namespace(task_name(name), &blk)
        self
      end

      def apply(&blk)
        instance_eval(&blk)
        self
      end

      def style(obj, *args, target: nil, empty: false)
        data = nil
        if target
          Array(target).each_with_index do |key, i|
            if i == 0
              break unless (data = __get__(:theme)[key.to_sym])
            else
              data = data[key.to_sym] ||= {}
            end
          end
        end
        if obj.is_a?(String)
          obj = begin
            JSON.parse(homepath(obj).read, { symbolize_names: true })
          rescue StandardError => e
            warn log_message(Logger::ERROR, e)
          end
        end
        apply_style(data || theme, obj, args, empty: empty) if obj && (!target || data)
        self
      end

      def describe(data)
        @describe ||= {
          alias: {},
          replace: [],
          pattern: {}
        }
        data.each do |key, val|
          key = key.to_s
          if key.start_with?(/(\\A|\^)/) || key.match?(/(\\z|\$)\z/)
            @describe[:replace] << [Regexp.new(key), val]
          else
            @describe[val.is_a?(Regexp) ? :pattern : :alias][key] = val
          end
        end
        self
      end

      def find(path = nil, name: nil, group: nil, ref: nil, &blk)
        return @project.values.find(&blk) if block_given? && !path && !name && !group && !ref

        ret = group ? select { |item| item.group == group } : []
        if path.is_a?(Symbol)
          ref ||= path
          path = nil
        end
        if ret.empty?
          ret = select { |item| item.ref?(ref) } if ref
          if ret.empty? && (path || name)
            path &&= rootpath path
            name &&= name.to_s
            if (proj = find { |item| item.path == path || item.name == name })
              ret << proj
            end
          end
        end
        return (group || ref ? ret : ret.first) unless block_given?

        ret.each(&blk)
        self
      end

      def get(name, &blk)
        ret = @project[name.to_s]
        return ret unless block_given?

        ret&.instance_eval(&blk)
        self
      end

      def find_base(obj)
        Application.kind_project.find { |proj| obj.instance_of?(proj) }
      end

      def task_name(val)
        prefix ? task_join(prefix, val) : val.to_s
      end

      def task_localname(val)
        prefix && val.is_a?(String) ? val.sub(/^#{Regexp.escape(prefix)}:/, '') : val.to_s
      end

      def task_desc(*args, **kwargs)
        return unless TASK_METADATA

        name = kwargs.delete(:name)
        if @describe
          val = name || task_join(*args)
          found = false
          sub = lambda do |data, out|
            index = data.size
            data.to_a.reverse_each { |group| out.sub!("%#{index -= 1}", group) }
            out
          end
          @describe[:replace].each do |pat, tmpl|
            next unless val =~ pat

            val = sub.call($~, tmpl.dup)
            found = true
          end
          if (out = @describe[:alias][val])
            val = out
            found = true
          else
            @describe[:pattern].each do |key, pat|
              next unless val =~ pat

              val = sub.call($~, key.dup)
              found = true
              break
            end
          end
          args = split_escape(val, char: ':').map! { |s| s.gsub('\\:', ':') } if found
        end
        desc message(*args, **kwargs)
      end

      def task_namespace(val, first: false)
        return unless (ret = val.to_s.split(':')).size > 1

        first ? ret.first : task_join(*ret[0..-2])
      end

      def task_resolve(obj, key)
        tasks = []
        if (base = task_base?(key))
          tasks << key if obj.has?(key, baseref)
        elsif (batch = series.batch_get(key))
          obj.allref do |ref|
            next unless obj.has?(key, ref) && (data = batch[ref])

            data.each do |val|
              if (items = task_resolve(obj, val)).empty?
                tasks.clear
                break
              end
              tasks.concat(items)
            end
            return tasks unless tasks.empty?
          end
        elsif task_extend?(obj, key)
          tasks << key
        end
        ret = []
        if tasks.empty?
          return [] if (base && !obj.ref?(baseref)) || !(data = series.alias_get(key))

          obj.allref do |ref|
            next unless obj.has?(key, ref) && (alt = data[ref])

            ret = task_resolve obj, alt
            break unless ret.empty?
          end
        else
          tasks.each do |val|
            target = task_join(obj.name, series.name_get(val))
            return [] unless task_defined?(target)

            ret << target
          end
        end
        ret
      end

      def task_sync(key)
        key = task_name key
        task_defined?(ret = task_join(key, 'sync')) ? ret : key
      end

      def format_desc(val, opts = nil, arg: 'opts*', before: nil, after: nil, out: false)
        return unless TASK_METADATA

        val = val.split(':') if val.is_a?(String)
        if before || after || opts
          pos = []
          pos << (before.is_a?(Array) ? before.join(',') : before) if before
          if opts
            pos << if opts.is_a?(Array)
                     arg ? "#{arg}=#{opts.join(',')}" : opts.join('|')
                   else
                     opts
                   end
          end
          pos << (after.is_a?(Array) ? after.join(',') : after) if after
          val << "#{val.pop}[#{pos.join(',')}]"
        end
        out ? message(*val) : task_desc(*val)
      end

      def script_find(*args)
        args.reverse_each do |val|
          if val && (ret = val.is_a?(Symbol) ? @script[:ref!][val] : @script[:group!][val.to_sym])
            return ret
          end
        end
        @script[:ref!][:''] ||= scriptobj
      end

      def script_get(*args, group: nil, ref: nil)
        data_get(*args, group: group, ref: ref, target: @script)
      end

      def events_get(*args, group: nil, ref: nil)
        data_get(*args, group: group, ref: ref, target: @events)
      end

      def banner_get(*ref, group: nil)
        ret = nil
        return ret if group && (ret = @banner[:group][group.to_sym])

        ref.reverse_each { |val| return ret if (ret = @banner[:ref][val]) }
        @banner[:ref][:_]
      end

      def enabled?
        !@extensions.empty? || any? { |proj| proj.enabled?(base: false) }
      end

      def task_base?(key)
        series.base?(key)
      end

      def task_extend?(obj, key)
        series.extend?(obj, key)
      end

      def task_include?(obj, key, ref = nil)
        return false if series.exclude?(key)

        task_base?(key) ? obj.has?(key, ref || baseref) : task_extend?(obj, key)
      end

      def task_exclude?(key, obj = nil)
        if obj
          data = obj.group ? @pass[:group][obj.group] : @pass[:ref][obj.ref]
          blk = (data && data[key.to_sym]) || @pass[:global][key.to_sym]
          return true if blk && obj.instance_eval(&blk)

          key = task_join(task_localname(obj.name), key)
        end
        @pass[:pattern].any? { |item| item.is_a?(Regexp) ? key.to_s.match?(item) : key == item }
      end

      def task_defined?(*key)
        Rake::Task.task_defined?(key.size == 1 ? key.first : task_join(*key))
      end

      def dev?(**kwargs)
        script?(:dev, **kwargs)
      end

      def prod?(**kwargs)
        script?(:prod, **kwargs)
      end

      def home?
        !!find(home)&.enabled?
      end

      def windows?
        Rake::Win32.windows?
      end

      def mri?
        RUBY_ENGINE == 'ruby'
      end

      def jruby?
        RUBY_ENGINE == 'jruby'
      end

      def truffleruby?
        RUBY_ENGINE == 'truffleruby'
      end

      def docker?
        !Dir['/.dockerenv', '/docker-*.{sh,d}'].empty?
      end

      def powershell?
        return true if ENV['SHELL']&.end_with?(File.join('', 'pwsh'))
        return false unless windows?

        case ENV['TERM_PROGRAM']
        when 'powershell.exe', 'vscode'
          true
        else
          ENV.fetch('PSModulePath', '').split(';', 2).size > 1
        end
      end

      def rootpath(*args)
        root.join(*args)
      end

      def homepath(*args)
        home.join(*args)
      end

      def pwd
        Pathname.new(Rake.application.original_dir)
      end

      def baseref
        Application.baseref
      end

      def invokeargs
        { exception: exception, warning: warning }
      end

      def size
        @project.size
      end

      def to_s
        (home? ? home : root).to_s
      end

      def inspect
        "#<#{self.class}: #{main} => #{self}>"
      end

      public :task_join

      private

      def __build__(default: nil, **)
        unless task_defined?('squared:version')
          task 'squared:version' do
            puts Squared::VERSION
          end
        end
        if default && task_defined?(out = task_name(default))
          task Rake.application.default_task_name => out
        end
      end

      def __chain__(*)
        @chain.each do |key, group|
          level = []
          sync = []
          failed = []
          i = 0
          pass = nil
          until (i > 0 && !group.compact! && !pass) || group.empty?
            group.each_with_index do |data, j|
              if i == 0
                action, reject = data.action.partition { |val| task_defined?(val) }
                failed.concat(reject)
                next group[j] = nil if action.empty?

                step = data.step
                data.action = action
              else
                step = 0
                catch :found do
                  has = ->(c, d) { c.any? { |e| e.is_a?(Regexp) ? d.start_with?(e) : d == e } }
                  w = data.with
                  a = data.after
                  b = data.before
                  level.each_with_index do |tasks, k|
                    ac = lambda do |n|
                      tasks.insert(n, *data.action)
                      sync << tasks
                      data.action.clear
                    end
                    tasks&.each_with_index do |v1, l|
                      index = k if w && has.call(w, v1)
                      if a && has.call(a, v1)
                        if index
                          ac.call(l.succ)
                          throw :found
                        else
                          index = k.succ
                        end
                      elsif b && has.call(b, v1)
                        if index
                          ac.call(l)
                          throw :found
                        else
                          index = k.pred
                        end
                      elsif index
                        if a || b
                          tasks.each_with_index do |v2, m|
                            if a && has.call(a, v2)
                              ac.call(m.succ)
                              throw :found
                            elsif b && has.call(b, v2)
                              ac.call(m)
                              throw :found
                            end
                          end
                          if !pass
                            pass = [i, data]
                          elsif pass.include?(data)
                            if i == pass.first.succ
                              pass.delete(data)
                              pass = nil if pass.size == 1
                            end
                          else
                            pass << data
                          end
                          next
                        end
                      else
                        next
                      end
                      step = index == -1 ? -1 : index.succ
                      throw :found
                    end
                  end
                end
              end
              if step == -1
                level.unshift(data.action)
                step = 0
              elsif step > 0
                (level[step -= 1] ||= []).concat(data.action)
              elsif !data.action.empty?
                next
              end
              sync << level[step] if data.sync
              group[j] = nil
              pass = nil
            end
            i += 1
          end
          level.compact!
          sync.uniq!
          series.chain(key, level, sync: sync)
          next if task_defined?(key = task_join(key, 'print'))

          format_desc key
          task key do
            unless failed.empty? && group.empty?
              group.each { |val| failed += val.action }
              puts log_message(Logger::ERROR, *failed, subject: 'failed placement', hint: false), pipe: 2
            end
            level.each_with_index do |grp, i|
              title = "Step #{i.succ}#{if sync.include?(grp) && !(grp.size == 1 && series.parallel.include?(grp.first))
                                         ' (sync)'
                                       end}"
              emphasize(grp, title: title, cols: level.flatten(1).push(title), border: theme[:border],
                             sub: opt_style(theme[:header], /\A(Step \d+)(.*)\z/))
            end
          end
        end
      end

      def puts(*args, **kwargs)
        log_console(*args, pipe: kwargs[:pipe] || pipe)
      end

      def script_command(task, val, group, ref, on, &blk)
        if block_given?
          val = Struct::RunData.new(val, blk)
        elsif !val
          return self
        end
        if group
          label = :group
          as_a group, :to_sym
        else
          label = :ref
          as_a ref, :to_sym
        end.each do |name|
          @script[label][name][task] = val
          @events[label][name][task] = on if on.is_a?(Hash)
        end
        self
      end

      def script_set(data, group: nil, ref: nil)
        data.freeze
        if group
          Array(group).each { |val| @script[:group!][val.to_sym] = data }
        elsif ref
          Array(ref).each { |val| @script[:ref!][val.to_sym] = data }
        else
          @script[:ref!][:''] = data
        end
      end

      def data_get(*args, group: nil, ref: nil, target: nil)
        if group && target[:group].key?(key = group.to_sym)
          target[:group][key]
        elsif ref.is_a?(Enumerable)
          ref.each do |key|
            next unless target[:ref].key?(key)

            ret = target[:ref][key]
            return ret if args.empty? || args.any? { |val| ret.key?(val) }
          end
          nil
        elsif ref && target[:ref].key?(ref)
          target[:ref][ref]
        end
      end

      def require_project(ref)
        return unless ref.is_a?(Symbol) && Application.kind_project.none? { |proj| proj.ref == ref }

        name = ref.to_s
        Application.load_project.each do |val|
          next unless File.exist?("#{rb = File.join(val, name)}.rb")

          require_relative rb
          break
        end
      end

      def root?(path, pass: [])
        return false unless path.directory?

        path.each_child do |c|
          name = c.basename.to_s
          unless c.to_s == __FILE__ || (@main == name && c.directory? && c.empty?) || pass.any? { |val| val == name }
            return false
          end
        end
        true
      end

      def script?(state, target: nil, pat: nil, group: nil, ref: baseref, global: false, script: true)
        data = script_find ref, group
        type = script ? :script : :run
        if global
          target = data[type] if target.nil?
          pat = data[state] if pat.nil?
        end
        return false if state == :prod && data[:dev] == true && data[:global][type]

        target && pat.is_a?(Regexp) ? Array(target).any?(pat) : pat == true
      end

      def scriptobj
        {
          run: nil,
          script: nil,
          dev: nil,
          prod: nil,
          global: {},
          env: {}
        }
      end
    end

    class Series
      include Rake::DSL
      extend Forwardable

      TASK_BASE = []
      TASK_BATCH = {}
      TASK_EXTEND = Support.hashlist
      TASK_METHODS = {}
      TASK_KEYS = []
      TASK_ALIAS = Support.hashobj
      TASK_NAME = {}
      private_constant :TASK_BASE, :TASK_BATCH, :TASK_EXTEND, :TASK_METHODS, :TASK_KEYS, :TASK_ALIAS, :TASK_NAME

      class << self
        def add(task, obj)
          key_set task
          TASK_EXTEND[task] << obj
        end

        def batch(*args, obj)
          if obj.is_a?(Hash)
            obj.each do |key, val|
              key_set key
              data = TASK_BATCH[key] ||= {}
              args.each { |ref| (data[ref] ||= []).concat(val) }
            end
          elsif (data = TASK_BATCH[obj])
            args.each { |ref| data.delete(ref) }
            if data.empty?
              TASK_KEYS.delete(obj)
              TASK_BATCH.delete(obj)
            end
          end
        end

        def alias(ref, obj)
          if obj.is_a?(Hash)
            obj.each { |key, val| TASK_ALIAS[key][ref] = val }
          elsif TASK_ALIAS.key?(obj)
            TASK_ALIAS[obj].delete(ref)
            TASK_ALIAS.delete(obj) if TASK_ALIAS[obj].empty?
          end
        end

        def rename(key, task)
          TASK_NAME[key.to_sym] = task.to_sym
        end

        def base_set(obj)
          TASK_BASE.clear
          TASK_BASE.concat(obj.tasks.reject { |val| TASK_KEYS.include?(val) })
        end

        def extend_set(obj)
          obj.tasks&.each { |task| add(task, obj) }
          TASK_METHODS[obj] = obj.instance_methods(false)
        end

        private

        def key_set(val)
          return if TASK_KEYS.include?(val)

          TASK_KEYS << val
          TASK_BASE.delete(val)
        end
      end

      attr_reader :sync, :multiple, :parallel

      def_delegators :@data, :[], :each, :each_key, :keys, :key?, :include?, :fetch, :update, :merge!, :any?, :none?,
                     :delete, :delete_if, :find, :find_all, :to_a, :to_s, :inspect
      def_delegators :@workspace, :task_desc, :task_name, :task_namespace, :task_join, :format_desc

      def initialize(workspace, exclude: [])
        @workspace = workspace
        @sync = []
        @multiple = []
        @parallel = []
        @chain = {}
        @exclude = exclude.freeze
        @session = {
          group: Support.hashlist,
          parent: Support.hashlist,
          id: []
        }
      end

      def populate(proj, **)
        group, parent, id = @session.values
        ws = proj.workspace
        each do |key, items|
          next if exclude?(key) || (tasks = ws.task_resolve(proj, key)).empty?

          if (g = proj.group)
            id << g
            group[:"#{key}:#{g}"].concat(tasks)
          else
            items.concat(tasks)
          end
          if tasks.size > 1 && batch?(proj, key) && !ws.task_exclude?(key, proj)
            ws.task_desc(t = ws.task_join(proj.name, key))
            task t => tasks
          end
          next unless (b = ws.find_base(proj)) && (n = b.ref.to_s) != g

          id << n
          parent[:"#{key}:#{n}"].concat(tasks)
        end
      end

      def build(parallel: [], pattern: [], **)
        subcheck = ->(val) { (ns = task_namespace(val)) && parallel.include?(ns) }
        update @session[:parent] if @session[:id].uniq.size > 1
        update @session[:group]
        each do |key, items|
          next if exclude?(key, true) || @workspace.task_exclude?(t = name_get(key))

          key = task_name t
          title = format_desc(key, out: true)
          if items.size > 1
            @multiple << key
            if parallel.include?(t) || pattern.any? { |pat| t.match?(pat) } || subcheck.call(t)
              task_desc("#{title} (thread)", name: key) if title
              multitask key => items
              @parallel << key

              s = task_join key, 'sync'
              task_desc("#{title} (sync)", name: s) if title
              task s => items
              @sync << s
              next
            end
          end

          task_desc(title, name: key) if title
          task key => items
        end
        @multiple.concat(sync)
      end

      def chain(key, level, sync: [])
        return if level.empty?

        index = 0
        prereqs = level.map do |tasks|
          task_join(key, index += 1).tap do |subkey|
            if sync.include?(tasks) || (tasks.size == 1 && (sync << tasks))
              task subkey => tasks
            else
              multitask subkey => tasks
            end
          end
        end
        @chain[key] = level.freeze
        parallel << key

        format_desc key, level.map(&:size).join('-')
        task key => prereqs
      end

      def reset
        @data = {}
        (TASK_BASE + TASK_KEYS).each { |key| @data[key] = [] }
        @data
      end

      def name_get(key)
        (TASK_NAME[key] || key).to_s
      end

      def batch_get(key)
        TASK_BATCH[key]
      end

      def alias_get(key)
        return unless TASK_ALIAS.key?(key)

        TASK_ALIAS[key]
      end

      def some?(key)
        return key?(key) && !self[key].empty? unless (batch = batch_get(key))

        batch.each_value do |items|
          return true if items.all? { |val| some?(val) || alias_get(val)&.any? { |_, alt| some?(alt) } }
        end
        false
      end

      def base?(key)
        TASK_BASE.include?(key)
      end

      def extend?(obj, key)
        return false unless TASK_EXTEND.key?(key)

        ret = false
        TASK_EXTEND[key].each do |kind|
          next unless obj.is_a?(kind)

          meth = :"#{key}?"
          if TASK_METHODS[kind].include?(meth)
            out = obj.__send__(meth)
            return true if out == 1
            return out if obj.ref?(kind.ref)
          elsif obj.ref?(kind.ref)
            ret = true
          end
        end
        ret
      end

      def batch?(obj, key)
        return false unless (data = batch_get(key))

        data.keys.any? { |ref| obj.ref?(ref) }
      end

      def chain?(val)
        @chain.each_value do |tasks|
          tasks.flatten(1).each do |name|
            next unless (task = invoked_get(name))

            return true if name == val || task.prerequisites.any? { |pr| pr == val && Rake::Task[pr].already_invoked }
          end
        end
        false
      end

      def multiple?(val = nil)
        already_invoked? multiple, val
      end

      def sync?(val = nil)
        already_invoked? sync, val
      end

      def parallel?(val = nil)
        already_invoked? parallel, val
      end

      def exclude?(key, empty = false)
        @exclude.include?(key) || (empty && (!key?(key) || self[key].empty?))
      end

      private

      def invoked_get(name)
        return unless Rake::Task.task_defined?(name) && (ret = Rake::Task[name]).already_invoked

        ret
      end

      def already_invoked?(list, val)
        if val
          list.include?(val) && !invoked_get(val).nil?
        else
          Rake::Task.tasks.any? { |obj| obj.already_invoked && list.include?(obj.name) }
        end
      end
    end

    Application.impl_series = Series

    module Project
      module Support
        class OptionPartition
          include Common::Shell
          extend Forwardable

          OPT_NAME = /\A(?:(--)|-)((?(1)[^=\s-][^=\s]*|[^=\s-]))\z/
          OPT_VALUE = /\A-{0,2}([^=\s-][^=\s]*)(?:=|\s+)(\S.*)\z/
          OPT_SINGLE = /\A-([^=\s-])(.+)\z/
          private_constant :OPT_NAME, :OPT_VALUE, :OPT_SINGLE

          class << self
            include Common::Format
            include Shell
            include Prompt

            def append(target, *args, delim: false, escape: false, quote: true, strip: nil, force: true, double: false,
                       filter: nil, **)
              return if (ret = args.flatten).empty?

              target << '--' if delim && !target.include?('--')
              if strip
                pat, s = Array(strip)
                ret.map! { |val| val.is_a?(String) ? val.gsub(pat, s || '') : val }
              end
              ret, err = ret.partition { |val| filter.match?(val.to_s) } if filter
              if block_given?
                out = []
                err ||= []
                ret.each do |val|
                  case (s = yield val)
                  when String
                    out << s
                  when NilClass, FalseClass
                    err << val
                  else
                    out << val
                  end
                end
                ret = out
              end
              ret.map! do |val|
                next val if opt?(val)

                if quote || val.is_a?(Pathname)
                  shell_quote(val, force: force, double: double)
                elsif escape
                  shell_escape(val, quote: quote, double: double)
                else
                  val
                end
              end
              if target.is_a?(Set)
                target.merge(ret)
              else
                target.concat(ret)
              end
              err || ret
            end

            def clear(target, opts, pass: true, styles: nil, **kwargs)
              return if opts.empty?

              kwargs[:subject] ||= target.first.stripext
              kwargs[:hint] ||= 'unrecognized'
              append(target, opts, delim: true) if kwargs.delete(:append)
              warn log_warn(opts.join(', '), pass: true, **kwargs)
              exit 1 unless pass || confirm("Run? [#{sub_style(target, styles)}]", 'N')
            end

            def delete_key(target, *args, value: false, reverse: false, count: -1)
              ret = []
              args.each do |val|
                next if (opts = target.grep(matchopt(val, value))).empty?

                opts = opts.first(count) if count >= 0
                opts.send(reverse ? :reverse_each : :each) { |key| target.delete(key) }
                ret.concat(opts)
              end
              ret
            end

            def strip(val)
              val = shell_split val if val.is_a?(String)
              Array(val).map { |s| s.sub(OPT_SINGLE, '\1=\2').sub(OPT_VALUE, '\1=\2').sub(OPT_NAME, '\2') }
                        .reject(&:empty?)
            end

            def select(list, bare: true, no: true, single: false, double: false)
              ret = bare ? list.grep_v(/=/) : list.grep(/=/).map! { |val| val.split('=', 2).first }
              ret.map! { |val| val.split('|', 2).last }
              ret = ret.grep_v(/\Ano-/) unless no
              return ret if single == double

              ret.select { |val| single ? val.size == 1 : val.size > 1 }
            end

            def uniq!(list, pass = [])
              keys = {}
              list.each_with_index do |val, i|
                j = val =~ OPT_VALUE ? $1 : val
                (keys[j] ||= []) << i unless pass.include?(j)
              end
              data = keys.map { |item| item[1].size > 1 ? item[1][0..-2] : [] }.reject(&:empty?)
              return if data.empty?

              data.each { |key| key.each { |i| list[i] = nil } }
              list.compact!
              list
            end

            def parse_arg!(name, val)
              return unless val.is_a?(String)

              a, b = name.size == 1 ? %w[- *] : %w[(?:--) +]
              return unless val =~ /\A#{a}?#{Regexp.escape(name)}(=|\s#{b})(["'])?(.+)(?(2)\2\z|\z)/m

              [name, $3, $2 || (name.size == 1 && $1.empty? ? true : '')]
            end

            def arg?(target, *args, value: false, **)
              r, s = args.partition { |val| val.is_a?(Regexp) }
              r << matchopts(s, value) unless s.empty?
              a = target.to_a
              if (n = a.index('--'))
                a = a[0..n]
              end
              r.any? { |pat| a.any?(pat) }
            end

            def opt?(val)
              return false unless val.is_a?(String)

              val.start_with?('-') && (OPT_NAME.match?(val) || OPT_VALUE.match?(val) || OPT_SINGLE.match?(val))
            end

            def pattern?(val)
              val.match?(/\A\^|\$\z/) || val.match?(/[.)][*+?]|\(\?:|\\[dsw\d]|\[.+\]|\{\d+,?\d*\}/i)
            end

            private

            def matchopt(val, value = false)
              /\A#{val.size == 1 ? shortopt(val) : longopt(val, value)}/
            end

            def matchopts(list, value = false)
              a, b = Array(list).partition { |val| val.size == 1 || val.match?(OPT_SINGLE) }
              return /\A#{shortopt(*a)}/ if b.empty?
              return /\A#{longopt(*b, value)}/ if a.empty?

              /\A(?:#{shortopt(*a)}|#{longopt(*b, value)})/
            end

            def shortopt(*group)
              group.map! { |s| Regexp.escape(s.delete_prefix('-')) }
              "-(?:#{group.join('|')})(?:\\z|[^ =]| +[^ -])"
            end

            def longopt(*group, value)
              group.map! { |s| Regexp.escape(s.delete_prefix('--')) }
              "--(?:#{group.join('|')})(?:#{value ? '=[^ ]| +[^ -]' : '[= ]|\z'})"
            end
          end

          attr_reader :target, :extras, :found, :errors, :values, :project, :path, :sep

          def_delegators :@target, :+, :-, :<<, :any?, :none?, :include?, :add, :add?, :find, :find_all, :find_index,
                         :merge, :compact, :delete, :delete?, :delete_if, :grep, :grep_v, :inspect, :to_a, :to_s
          def_delegators :@extras, :empty?, :member?, :each, :each_with_index, :each_with_object, :partition, :dup,
                         :first, :shift, :unshift, :pop, :push, :concat, :index, :join, :detect, :map, :map!, :select,
                         :select!, :reject, :slice, :slice!, :size

          def_delegator :@extras, :delete, :remove
          def_delegator :@extras, :delete_at, :remove_at
          def_delegator :@extras, :delete_if, :remove_if
          def_delegator :@extras, :find_all, :detect_all
          def_delegator :@extras, :find_index, :detect_index

          def initialize(opts, list, target = JoinSet.new, project: nil, path: nil, sep: '=', **kwargs, &blk)
            @target = target.is_a?(Set) ? target : target.to_set
            @project = project
            @path = path || project&.path
            @sep = sep
            @errors = []
            @found = []
            parse(list, opts, **kwargs, &blk)
          end

          def parse(list, opts = extras, no: nil, single: nil, args: false, multiple: nil, first: nil, underscore: nil,
                    stdin: false, &blk)
            @extras = []
            @values = []
            bare = []
            e = []
            b = []
            m = []
            p = []
            q = []
            qq = []
            i = []
            f = []
            si = []
            bl = []
            ml = []
            list.flat_map do |val|
              x, y = val.split('|', 2)
              if y
                if (n = val.index('='))
                  x += val[n..-1]
                end
                [x, y]
              else
                x
              end
            end
            .each do |val|
              if (n = val.index('='))
                flag = val[0, n]
                case val[n.succ]
                when 'e'
                  e << flag
                when 'b'
                  b << flag
                when 'm'
                  m << flag
                when 'q'
                  qq << flag if val[n + 2] == 'q'
                  q << flag
                when 'p'
                  p << flag
                when 'i'
                  i << flag
                when 'f'
                  f << flag
                when 'n'
                  si << flag
                when 'v'
                  @values << Regexp.escape(flag)
                when '!'
                  bl << flag
                when '+'
                  ml << flag
                  bare << flag
                else
                  next
                end
                m << flag if val[n + 2] == 'm'
                bare << flag if val.end_with?('?')
              else
                bare << val
              end
            end
            no = (no || []).map { |val| (n = val.index('=')) ? val[0, n] : val }
            bare.concat(no)
            if underscore
              tr = ->(a) { a.map { |val| val.tr('-', '_') } }
              @values.concat(tr.call(@values))
              bare.concat(tr.call(bare))
              e.concat(tr.call(e))
              b.concat(tr.call(b))
              m.concat(tr.call(m))
              p.concat(tr.call(p))
              q.concat(tr.call(q))
              qq.concat(tr.call(qq))
              i.concat(tr.call(i))
              f.concat(tr.call(f))
              si.concat(tr.call(si))
              bl.concat(tr.call(bl))
              ml.concat(tr.call(ml))
              no.concat(tr.call(no))
            end
            if target.is_a?(JoinSet)
              target.multiple = multiple if multiple
              target.multiple = ml.map { |val| val.size == 1 ? "-#{val}" : "--#{val}" }
            end
            numtype = [
              [i, /\A\d+\z/],
              [f, /\A\d*(?:\.\d+)?\z/],
              [si, /\A-?\d+\z/]
            ].freeze
            numcheck = ->(k, v) { numtype.any? { |flag, pat| flag.include?(k) && v.match?(pat) } }
            skip = false
            opts.each do |opt|
              if stdin
                if stdin == -1
                  add_path opt if exist?(opt)
                  next
                elsif opt == '-'
                  add '-'
                  stdin = -1
                  next
                end
              end
              next skip = true if opt == '--'
              next push opt if skip

              if single&.match?(opt)
                add "-#{opt}"
              elsif bare.include?(opt)
                add(opt.size == 1 ? "-#{opt}" : "--#{opt}")
              elsif opt.start_with?(/no[-_]/) && no.include?(name = opt[3..-1])
                add "--no-#{name}"
              else
                if opt =~ OPT_VALUE
                  key = $1
                  val = $2
                  merge = m.include?(key)
                  if e.include?(key)
                    add shell_option(key, val, merge: merge, sep: sep)
                  elsif q.include?(key)
                    add quote_option(key, val, double: qq.include?(key), merge: merge, sep: sep)
                  elsif p.include?(key)
                    if val.match?(/\A(["']).+\1\z/)
                      add shell_option(key, val, escape: false, merge: merge, sep: sep)
                    elsif path
                      add quote_option(key, path + val, merge: merge, sep: sep)
                    else
                      push opt
                    end
                  elsif b.include?(key) || (bl.include?(key) && %w[true false].include?(val)) || numcheck.call(key, val)
                    add basic_option(key, val, merge: merge, sep: sep)
                  elsif merge
                    add basic_option(key, val, merge: true, sep: sep)
                  else
                    push opt
                  end
                  opt = key
                else
                  push opt
                  skip = true if args
                end
                skip = true if first&.any? { |s| s.is_a?(Regexp) ? opt.match?(s) : !opt.include?(s) }
              end
            end
            @values = @values.empty? ? /\A\s+\z/ : /\A(#{@values.join('|')})#{sep}(.+)\z/m
            @extras.each_with_index(&blk) if block_given?
            self
          end

          def swap(opts = nil, &blk)
            unless opts
              opts = found
              @found = []
            end
            opts.sort!(&blk) if block_given?
            @extras = opts
            self
          end

          def append(*args, **kwargs, &blk)
            args = extras if args.empty?
            out = OptionPartition.append(target, *args, **kwargs, &blk)
            errors.concat(out) if out && (block_given? || kwargs[:filter])
            self
          end

          def append_any(*args, escape: false, quote: true, **kwargs)
            (args.empty? ? extras : args.flatten).each do |val|
              if block_given?
                temp = val
                val = yield val
                if val.is_a?(Array)
                  found << temp
                  k, v, q = val
                  add_option(k, v, escape: escape, quote: quote, double: q == '"', merge: q == true, **kwargs)
                  next
                end
              end
              next unless val.is_a?(String)

              if exist?(val)
                add_path(val, **kwargs)
              elsif quote
                add_quote(val, **kwargs)
              elsif escape
                add shell_escape(val, **kwargs)
              else
                add val
              end
              found << (temp || val) if args.empty?
            end
            self
          end

          def delete_key(*args, **kwargs)
            OptionPartition.delete_key(target, *args, **kwargs)
            self
          end

          def values_of(*args, strict: true, first: false, last: false)
            eq, s = strict ? [sep, '[^ ]+'] : ['(?:=| +)', '[^-][^ ]*']
            g = ["\"((?:[^\"]|(?<=\\\\)\"(?!$#{'| ' if windows?}))*)\""]
            g << "'((?:[^']|'\\\\'')*)'" unless windows?
            g << "(#{s})"
            args.map! do |opt|
              if opt.size == 1
                /(?:\A| )-#{opt} ?([^ ]+)/
              else
                /(?:\A| )--#{opt + eq}(?:#{g.join('|')})/
              end
            end
            ret = []
            target.each do |opt|
              args.each do |pat|
                next unless opt =~ pat

                ret << ($1 || $2 || $3)
                break
              end
            end
            return ret unless first || last

            if last.is_a?(Numeric)
              ret.last(last)
            elsif last
              ret.last
            else
              first.is_a?(Numeric) ? ret.first(first) : ret.first
            end
          end

          def uniq(list)
            ignore = map { |val| nameonly(val) }
            list.reject { |val| ignore.include?(s = nameonly(val)) || any?(OptionPartition.send(:matchopt, s)) }
          end

          def clear(opts = nil, errors: false, **kwargs)
            styles = project.theme[:inline] if project
            if errors
              OptionPartition.clear(target, @errors, styles: styles, **kwargs)
              @errors.clear
              return self unless opts
            end
            opts ||= extras
            OptionPartition.clear(target, opts - found, styles: styles, **kwargs)
            opts.clear
            self
          end

          def adjoin(*args, with: nil, start: false)
            index = -1
            temp = compact
            if with
              pat = case with
                    when String, Symbol
                      /\A#{Regexp.escape(with)}\z/
                    when Array
                      OptionPartition.send(:matchopts, with)
                    else
                      with
                    end
              temp.each_with_index do |val, i|
                if val.to_s.match?(pat)
                  index = i + (start.is_a?(Numeric) ? start : 1)
                  break
                end
              end
            else
              temp.each_with_index do |val, i|
                if index == 0
                  next unless val.is_a?(String) && val.start_with?('-')

                  index = i
                  break
                elsif i > 0 && !val.to_s.start_with?('-')
                  if start
                    index = i + (start.is_a?(Numeric) ? start : 1)
                    break
                  end
                  index = 0
                end
              end
            end
            if index > 0
              if args.empty?
                args = dup
                reset
              else
                args.each { |val| remove val }
              end
              args = temp[0, index] + args + temp[index..-1]
              target.clear
            end
            merge args
            self
          end

          def add_path(*args, option: nil, force: true, double: false, **kwargs)
            if args.empty?
              args = select { |val| val.is_a?(String) }
              found.concat(args)
              args.map! { |val| path + val } if path
              append(args, force: force, **kwargs)
            else
              val = path ? path.join(*args) : File.join(*args)
              if option
                add quote_option(option, val, double: double)
              else
                add shell_quote(val, option: false, force: force, double: double)
              end
            end
            self
          end

          def add_quote(*args, **kwargs)
            merge(args.compact
                      .map! { |val| val == '--' || OptionPartition.opt?(val) ? val : shell_quote(val, **kwargs) })
            self
          end

          def add_option(flag, val = nil, **kwargs)
            add shell_option(flag, val, **kwargs)
            self
          end

          def add_first(fallback = nil, prefix: nil, path: false, escape: false, quote: false, reverse: false,
                        expect: false, **kwargs)
            val = (reverse ? pop : shift) || fallback
            if val
              temp = val
              val = val.delete_prefix(prefix) if prefix && val.is_a?(String)
              unless block_given? && !(val = yield val).is_a?(String)
                if path
                  add_path(val, **kwargs)
                elsif quote
                  add_quote(val, **kwargs)
                elsif escape
                  add shell_escape(val, **kwargs)
                else
                  add val
                end
                found << temp unless temp == fallback
              end
            elsif expect
              raise(expect.is_a?(String) ? expect : 'no value queued')
            end
            self
          end

          def delim
            add '--'
            self
          end

          def last(val = nil, &blk)
            unless block_given?
              case val
              when NilClass
                return extras.last
              when Numeric
                return extras.last(val)
              when String, Array, Regexp
                val = OptionPartition.send(:matchopts, val) unless val.is_a?(Regexp)
                blk = proc { |s| s&.match?(val) }
              else
                raise TypeError, "unknown: #{val}"
              end
            end
            ret = find_all(&blk)
            unless ret.empty?
              ret = case val
                    when NilClass
                      ret.first(1)
                    when Numeric
                      ret.first(val)
                    else
                      ret
                    end
              ret.each do |opt|
                delete opt
                add opt
              end
            end
            val.nil? ? ret.first : ret
          end

          def splice(*exclude, quote: true, delim: true, path: false, pattern: false, &blk)
            temp, other = if block_given?
                            partition(&blk)
                          elsif exclude.first.is_a?(Symbol)
                            partition(&exclude.first)
                          else
                            exclude.map! { |pat| Regexp.new(pat) }
                            partition do |val|
                              val = val.to_s
                              next if pattern && OptionPartition.pattern?(val)

                              exclude.none? { |pat| val.match?(pat) }
                            end
                          end
            unless temp.empty?
              add '--' if delim
              extras.clear
              concat other
              if path
                temp.each { |val| add_path(val) }
              else
                temp.quote! if quote
                merge temp
              end
            end
            self
          end

          def reset(errors: false)
            extras.clear
            found.clear
            clear(errors: true) if errors
            self
          end

          def append?(key, val = nil, type: nil, force: false, sep: '=', **kwargs)
            return false unless force || !arg?(key)

            val = yield self if block_given?
            return false unless val

            type ||= :quote if kwargs.empty?
            add case type
                when :quote
                  quote_option(key, val, sep: sep)
                when :basic
                  basic_option(key, val, sep: sep)
                else
                  shell_option(key, val, sep: sep, **kwargs)
                end
            true
          end

          def arg?(*args, **kwargs)
            OptionPartition.arg?(target, *args, **kwargs)
          end

          def exist?(*args, add: false, first: false, last: false, glob: false)
            return with_glob?(File.join(*args), glob) unless args.empty?

            if first || last
              return false unless (val = first ? self.first : self.last)

              with_glob?(val, glob).tap do |ret|
                next unless add && ret

                add_first(path: true, reverse: !first)
              end
            else
              each_with_index do |val, i|
                next unless with_glob?(val, glob)

                if add
                  remove_at i
                  add_path val
                end
                return true
              end
              false
            end
          end

          def uniq!(list)
            unless (list = uniq(list)).empty?
              concat list
              self
            end
          end

          private

          def nameonly(val)
            val[OPT_VALUE, 1] || val
          end

          def with_glob?(val, glob = true)
            return false unless val.is_a?(String) && !val.empty?
            return File.exist?(val) unless path

            path.join(val).exist? || (glob && !path.glob(val).empty?)
          end

          def windows?
            require 'rake'
            Rake::Win32.windows?
          end
        end

        class JoinSet < Set
          def self.to_s
            super[/[^:]+\z/, 0]
          end

          alias to_ary to_a

          attr_reader :delim, :extras, :multiple

          def initialize(data = [], delim: ' ', partition: '--', uniq: /\A--?[^=\s-]/, multiple: [])
            @delim = delim
            @partition = partition
            @uniq = uniq
            @multiple = multiple
            @extras = []
            super(data.compact)
          end

          def multiple=(val)
            case val
            when Enumerable
              @multiple.concat(val.to_a.map { |val| val.is_a?(Regexp) ? val : val.to_s })
            when String, Symbol, Pathname
              @multiple << val.to_s
            when Regexp
              @multiple << val
            when NilClass, FalseClass
              @multiple.clear
            end
          end

          def insert(*args)
            replace Set.new(compact.insert(*args))
          end

          def slice(*args)
            compact.slice(*args)
          end

          def slice!(*args)
            data = compact
            data.slice!(*args).tap { replace Set.new(data) }
          end

          def compact
            dump to_ary
          end

          def last(val, pat)
            (@last ||= []).push([val, pat, $1]) if val =~ pat
            self << val
          end

          def pass(&blk)
            ret = compact
            @last&.each do |val, pat, key|
              items = []
              index = nil
              ret.each_with_index do |opt, i|
                if opt == val
                  index = i
                elsif index && opt[pat, 1] == key
                  items << i
                end
              end
              next unless index && !items.empty?

              val = ret[index]
              cur = index
              items.each do |k|
                ret[cur] = ret[k]
                cur = k
              end
              ret[items.last] = val
            end
            ret.concat(dump(extras)) unless extras.empty?
            return ret unless block_given?

            ret.reject(&blk)
          end

          def and(*args)
            self << '&&'
            merge args
          end

          def or(*args)
            self << '||'
            merge args
          end

          def with(*args, &blk)
            temp('&&', *args, &blk)
          end

          def temp(*args, &blk)
            args.compact!
            pass(&blk)
              .concat(args)
              .join(@delim)
          end

          def done
            to_s.tap { clear }
          end

          def merge(enum)
            if !extras.empty?
              extras.concat(enum.to_a)
              self
            elsif (n = enum.find_index { |val| extras?(val) })
              data = enum.to_a
              extras.concat(if n == 0
                              data
                            else
                              super(data[0, n])
                              data[n..-1]
                            end)
              self
            else
              super
            end
          end

          def <<(obj)
            return super if extras.empty? && !extras?(obj)

            unless !extras.include?(@partition) && include?(obj) && @uniq.match?(s = obj.to_s) && !multiple?(s)
              extras << obj
            end
            self
          end

          def size
            super + extras.size
          end

          def include?(obj)
            return true if super
            return extras.include?(obj) unless (n = extras.index(@partition))

            extras[0..n].include?(obj)
          end

          def multiple?(val)
            multiple.any? { |obj| obj.is_a?(Regexp) ? obj.match?(val) : obj == val }
          end

          def to_a
            pass
          end

          def to_s
            to_a.join(@delim)
          end

          def to_enum(*args)
            to_a.to_enum(*args)
          end

          def to_json(*args)
            to_a.to_json(*args)
          end

          def to_yaml(*args)
            to_a.to_yaml(*args)
          end

          alias add :<<
          alias add? :<<
          alias push :<<
          alias member? include?
          alias concat merge

          private

          def dump(enum)
            enum.map(&:to_s).reject(&:empty?)
          end

          def extras?(obj)
            obj == @partition || (include?(obj) && (!@uniq.match?(s = obj.to_s) || multiple?(s)))
          end
        end
      end

      class Base
        include Comparable
        include Common::Format
        include System
        include Shell
        include Prompt
        include Utils
        include Support
        include Workspace::Support::Variables
        include Rake::DSL

        OPTIONS = Workspace::Support.hashobj
        VAR_SET = %i[parent global script index envname desc dependfile dependname dependindex theme archive env graph
                     dev prod pass only exclude asdf].freeze
        BLK_SET = %i[run depend doc lint test copy clean].freeze
        SEM_VER = /\b(\d+)(?:(\.)(\d+))?(?:(\.)(\d+))?[-.]?(\S+)?\b/.freeze
        URI_SCHEME = %r{\A([a-z][a-z\d+-.]*)://[^@:\[\]\\^<>|\s]}i.freeze
        TASK_METADATA = Rake::TaskManager.record_task_metadata
        private_constant :OPTIONS, :VAR_SET, :BLK_SET, :SEM_VER, :URI_SCHEME, :TASK_METADATA

        class << self
          def populate(*); end
          def batchargs(*); end
          def aliasargs(*); end
          def bannerargs(*); end

          def tasks
            (%i[build archive graph prereqs] + BLK_SET).freeze
          end

          def options(*args, **kwargs)
            name = nil
            with = []
            proj = []
            opts = []
            args.each do |val|
              case val
              when String
                if name
                  opts << val
                else
                  name = val
                end
              when Symbol
                if name
                  proj << val
                else
                  with << val
                end
              end
            end
            return if !name || (opts.empty? && kwargs.empty? && with.empty?)

            base = OPTIONS[ref]
            data = [opts.freeze, kwargs.freeze, with.freeze].freeze
            proj << :_ if proj.empty?
            proj.each { |val| (base[val] ||= {})[name.to_s] = data }
          end

          def ref
            @ref ||= to_s.downcase.to_sym
          end

          def subtasks(val = nil, &blk)
            return @@tasks[val || ref].each(&blk) if block_given?

            @@tasks[ref] = val.freeze
          end

          def config?(*)
            false
          end

          def to_s
            super[/[^:]+\z/, 0]
          end

          private

          def as_path(val)
            case val
            when Pathname
              val
            when String
              Pathname.new(val)
            end
          end
        end

        @@tasks = {}
        @@graph = { _: [] }
        @@asdf = Pathname.new("#{Dir.home}/.asdf").yield_self do |path|
          version = if path.join('asdf.sh').exist?
                      15
                    elsif ENV['ASDF_DATA_DIR'] && (path = Pathname.new(ENV['ASDF_DATA_DIR'])).exist?
                      16
                    end
          Struct.new(:path, :version).new(path, version) if version
        end
        @@print_order = 0

        subtasks({
          'graph' => %i[run print].freeze,
          'unpack' => %i[zip gz tar ext],
          'asdf' => %i[set exec current update latest where reshim]
        })

        attr_reader :name, :workspace, :path, :theme, :group, :parent, :children, :dependfile,
                    :exception, :pipe, :verbose, :global
        attr_accessor :project

        def initialize(workspace, path, name, *, group: nil, first: {}, last: {}, error: {}, common: ARG[:COMMON],
                       **kwargs)
          @path = path
          @workspace = workspace
          @name = name.to_s.freeze
          @project = @path.basename.to_s.freeze
          @group = group&.to_s.freeze
          @envname = env_key(@name).freeze
          @depend = kwargs[:depend]
          @doc = kwargs[:doc]
          @lint = kwargs[:lint]
          @test = kwargs[:test]
          @copy = kwargs[:copy]
          @clean = kwargs[:clean]
          @release = kwargs[:release]
          self.version = kwargs[:version]
          self.exception = env_bool(kwargs[:exception], workspace.exception, strict: true)
          self.pipe = env_pipe(kwargs[:pipe], workspace.pipe, strict: true)
          self.verbose = case (val = env('VERBOSE', kwargs[:verbose]))
                         when String
                           env_bool(val, workspace.verbose, strict: true, index: true)
                         else
                           val.nil? ? workspace.verbose : val
                         end
          self.global = false
          @output = []
          @ref = []
          @children = []
          @events = hashobj.update({ first: first, last: last, error: error })
          @as = hashobj
          @desc = (@name.include?(':') ? @name.split(':').join(ARG[:SPACE]) : @name).freeze
          @log = nil
          @dev = nil
          @prod = nil
          @withargs = nil
          @session = nil
          @index = -1
          parent_set kwargs[:parent]
          run_set(kwargs[:run], kwargs[:env], opts: kwargs.fetch(:opts, true))
          graph_set kwargs[:graph]
          pass_set kwargs[:pass]
          only_set kwargs[:only]
          exclude_set kwargs[:exclude]
          archive_set kwargs[:archive]
          asdf_set kwargs[:asdf]
          theme_set common
          initialize_ref Base.ref
        end

        def initialize_ref(ref)
          @ref << ref unless @exclude.include?(ref)
        end

        def initialize_build(ref, **kwargs)
          initialize_ref ref
          if (@script = @workspace.script_get(group: @group, ref: ref))
            if @script[:log] && !kwargs.key?(:log)
              kwargs[:log] = @script[:log]
              @log = nil
            end
            @depend = @script[:depend] if @depend.nil?
            @doc = @script[:doc] if @doc.nil?
            @lint = @script[:lint] if @lint.nil?
            @test = @script[:test] if @test.nil?
            @clean = @script[:clean] if @clean.nil?
            @exclude = @script[:exclude] if @exclude.empty? && @script.key?(:exclude)
          end
          initialize_events(ref, **kwargs)
          initialize_logger(**kwargs)
          return if @output[0] == false

          data = @workspace.script_find(*@ref, @group)
          if @output[0].nil?
            if data[:script]
              unless kwargs[:script] == false
                script_set(data[:script], args: data.fetch(:args, kwargs[:args]), prod: kwargs[:prod], global: true)
              end
            elsif data[:run]
              run_set(data[:run], global: true)
            end
            if kwargs[:script]
              script_set(kwargs[:script], args: kwargs[:args]) unless data[:env][:script]
            elsif @script
              if @script[:script]
                script_set(@script[:script], args: @script.fetch(:args, kwargs[:args])) unless data[:global][:script]
              elsif @script[:run] && !data[:global][:run]
                run_set @script[:run]
              end
            end
          elsif data[:run] && data[:env][:run]
            run_set(data[:run], global: true)
          end
        end

        def initialize_events(ref, **)
          return unless (events = @workspace.events_get(group: @group, ref: ref))

          events.each { |task, data| data.each { |ev, blk| @events[ev][task] ||= [blk] } }
        end

        def initialize_logger(log: nil, **)
          return if @log

          log = log.is_a?(Hash) ? log.dup : { file: log }
          file = if (val = env('LOG_FILE'))
                   Time.now.strftime(val)
                 elsif (val = env('LOG_AUTO'))
                   require 'date'
                   "#{@name}-%s.log" % [case val
                                        when 'y', 'year'
                                          Date.today.year
                                        when 'm', 'month'
                                          Date.today.strftime('%Y-%m')
                                        when 'd', 'day', '1'
                                          Date.today
                                        else
                                          val.include?('%') ? Time.now.strftime(val) : Time.now.strftime('%FT%T%:z')
                                        end]
                 elsif (val = log[:file])
                   if val.is_a?(String)
                     Time.now.strftime(val)
                   else
                     require 'date'
                     "#{@name}-#{Date.today}.log"
                   end
                 end
                 .yield_self do |dir|
                   @workspace.home.join(env('LOG_DIR', ''), dir).realdirpath if dir
                 rescue StandardError => e
                   print_error e
                 end
          log[:progname] ||= @name
          env('LOG_LEVEL', ignore: false) { |val| log[:level] = val.start_with?(/\d/) ? log_sym(val.to_i) : val }
          log.delete(:file)
          @log = [file, log]
        end

        def initialize_env(dev: nil, prod: nil, **)
          @dev = env_match('BUILD', dev, suffix: 'DEV', strict: true)
          @prod = env_match('BUILD', prod, suffix: 'PROD', strict: true)
          env('BUILD', suffix: 'ENV') { |val| @output[2] = val if (val = parse_json(val)) } unless @output[0] == false
          unless @output[0] == false || @output[0].is_a?(Array)
            env('BUILD', suffix: 'OPTS') do |val|
              n = @output[0] ? 1 : 3
              @output[n] = merge_opts(@output[n], shell_split(val))
            end
            env(ref.to_s.upcase, suffix: 'OPTS') { |val| @output[4] = merge_opts(@output[4], shell_split(val)) }
          end
          env('BUILD', suffix: 'VERSION') { |val| self.version = val }
          env('BUILD', strict: true) do |val|
            if val == '0'
              @output = [false]
            elsif script?
              script_set val
            else
              run_set val
            end
          end
        end

        def ==(other)
          equal?(other)
        end

        def <=>(other)
          return unless workspace == other.workspace
          return 0 if equal?(other)

          a, b = graph_deps
          return 1 if a.include?(other)

          c, d = graph_deps other
          e = b - d
          f = d - b
          if parent == other.parent
            g = []
            h = []
          else
            g = a - c
            h = c - a
          end
          g << self
          h << other
          e.concat(g)
          f.concat(h)
          if g.any? { |val| f.include?(val) }
            -1
          elsif h.any? { |val| e.include?(val) }
            1
          elsif e.any? { |val| f.include?(val) } # rubocop:disable Lint/DuplicateBranch
            -1
          elsif f.any? { |val| e.include?(val) } # rubocop:disable Lint/DuplicateBranch
            1
          elsif @index >= 0 && (i = other.instance_variable_get(:@index)) >= 0
            @index <=> i
          end
        rescue StandardError => e
          log&.debug e
          nil
        end

        def version=(val)
          @version = val&.to_s
        end

        def exception=(val)
          @exception = case val
                       when Numeric, TrueClass, FalseClass
                         val
                       else
                         workspace.exception
                       end
        end

        def pipe=(val)
          @pipe = case val
                  when Numeric, Pathname
                    val
                  else
                    workspace.pipe
                  end
        end

        def verbose=(val)
          @verbose = case val
                     when Numeric, TrueClass, FalseClass
                       val
                     else
                       workspace.verbose
                     end
        end

        def global=(val)
          @global = val unless val.nil?
        end

        def ref
          Base.ref
        end

        def populate(keys, **)
          task_build keys
          return unless ref?(Base.ref)

          namespace name do
            Base.subtasks do |action, flags|
              next if task_pass?(action)

              namespace action do
                flags.each do |flag|
                  case action
                  when 'graph'
                    break unless graph?

                    format_desc action, flag, '(-)project*'
                    task flag do |_, args|
                      args = args.to_a.reject { |val| name == val }
                      if flag == :run
                        graph args
                      else
                        out = graph(args, out: [], order: {})
                        emphasize(out, title: path, right: true, border: borderstyle, sub: [
                          opt_style(theme[:header], /\A(#{Regexp.escape(path.to_s)})(.*)\z/),
                          opt_style(theme[:active], /\A(#{Regexp.escape(name)})(.*)\z/),
                          opt_style(theme[:inline], /\A((?~ \() \()(\d+)(\).*)\z/, 2)
                        ])
                      end
                    end
                  when 'unpack'
                    format_desc(action, flag, 'tag/url,dir,digest?,f/orce?', before: ('ext' if flag == :ext))
                    params = %i[tag dir digest force]
                    params.unshift(:ext) if flag == :ext
                    task flag, params do |_, args|
                      ext = flag == :ext ? param_guard(action, flag, args: args, key: :ext) : flag.to_s
                      tag = param_guard(action, flag, args: args, key: :tag)
                      dir = param_guard(action, flag, args: args, key: :dir)
                      unless tag.match?(URI_SCHEME)
                        tag = unpack_get tag, ext
                        tag ||= if @release
                                  "%s.#{ext}" % [@release.include?('??') ? @release.sub('??', tag) : @release + tag]
                                else
                                  raise_error ArgumentError, "no base uri: #{tag}", hint: ext
                                end
                      end
                      force = case (digest = args.digest)
                              when 'f', 'force'
                                digest = nil
                                true
                              else
                                args.fetch(:force, false)
                              end
                      unpack(basepath(dir), uri: tag, digest: digest, ext: ext, force: force)
                    end
                  when 'asdf'
                    break unless @asdf

                    case flag
                    when :set
                      format_desc action, flag, 'version,dir?=u/home|p/arent'
                      task flag, [:version] do |_, args|
                        args = if (version = args.version)
                                 args.extras
                               else
                                 version, opts = choice_index('Select a version',
                                                              @asdf[1].children
                                                                      .map(&:basename)
                                                                      .sort { |a, b| b <=> a }
                                                                      .push('latest', 'system'),
                                                              accept: [accept_y('Confirm?')],
                                                              values: 'Options')
                                 OptionPartition.strip(opts)
                               end
                        asdf(flag, args, version: version)
                      end
                    else
                      format_desc(action, flag, ('command' if flag == :exec))
                      task flag do |_, args|
                        args = args.to_a
                        args << readline('Enter command', force: true) if args.empty? && flag == :exec
                        asdf flag, args
                      end
                    end
                  end
                end
              end
            end
          end
        end

        def generate(keys, **)
          task_build keys
        end

        def with(**kwargs, &blk)
          @withargs = (kwargs unless kwargs.empty?)
          if block_given?
            instance_eval(&blk)
            @withargs = nil
          end
          self
        end

        def add(path, name = nil, **kwargs, &blk)
          if path.is_a?(String) && path =~ %r{\A(.+)[\\/]\*+\z}
            return self unless checkdir?(path = basepath($1))

            path = path.children.select { |val| checkdir?(val) }
          end
          if path.is_a?(Array)
            name = self.name if name == true
            path.each { |val| add(val, name && task_join(name, File.basename(val)), **kwargs, &blk) }
          elsif projectpath?(path = basepath(path)) && checkdir?(path)
            kwargs = hashdup(@withargs).update(kwargs) if @withargs
            kwargs[:group] = group if group && !kwargs.key?(:group)
            kwargs[:ref] = ref unless kwargs.key?(:ref)
            proj = nil
            name = case name
                   when String, Symbol
                     name.to_s
                   else
                     path.basename
                   end
            workspace.add(path, name, parent: self, **kwargs) do
              proj = self
              instance_eval(&blk) if block_given?
            end
            children << proj
          end
          self
        end

        def chain(*args, **kwargs)
          workspace.chain(*args, project: self, **kwargs)
          self
        end

        def inject(obj, *args, **kwargs, &blk)
          if enabled?
            raise 'link not compatible' unless obj.respond_to?(:link) && (out = obj.link(self, *args, **kwargs, &blk))

            out.build if out.respond_to?(:build)
          end
          self
        rescue StandardError => e
          print_error(e, subject: obj, hint: name)
          self
        end

        def build(*args, sync: invoked_sync?('build'), from: :run, **)
          banner = !silent?
          if args.empty?
            return unless from == :run

            banner = verbose? if from_base?('build')
            run_b(@run, sync: sync, banner: banner, from: from) if series?(@run)
            args = @output
          end
          if args.first.is_a?(Struct)
            f, blk = args.first.to_a
            args[0] = instance_eval(&blk) || f
            return unless args.first
          end
          if args.all? { |val| val.is_a?(Array) }
            cmd = []
            var = {}
            args.each do |val|
              case val.first
              when Proc
                instance_exec(*val[1..-1], &val.first)
                next
              when Method
                val.first.call(*val[1..-1])
                next
              end
              a, b, c, d, e = val
              case b
              when Hash
                b = append_hash(b, target: [], build: true).join(' ')
              when Enumerable
                b = b.to_a.join(' ')
              end
              d = append_hash(d, target: []).join(' ') if d.is_a?(Hash)
              if a
                cmd << [replace_bin(a), d, b].compact.join(' ')
              else
                next unless respond_to?(:compose)

                cmd << a if (a = compose(as_get(b, from), d, script: true, args: e, from: from))
              end
              var.update(c) if c.is_a?(Hash)
            end
            cmd = cmd.join(' && ')
          else
            cmd, opts, var, flags, extra = args
            if cmd
              return run_b(cmd, sync: sync, from: from) if cmd.is_a?(Proc) || cmd.is_a?(Method)

              cmd = replace_bin as_get(cmd, from)
              opts = compose(opts, script: false) if opts && respond_to?(:compose)
              flags = append_hash(flags, target: []).join(' ') if flags.is_a?(Hash)
              cmd = case opts
                    when Hash
                      [cmd, flags].concat(append_hash(opts, target: [], build: true))
                                  .compact
                                  .join(' ')
                    when Enumerable
                      cmd = Array(cmd).concat(opts.to_a)
                      cmd.map! { |val| "#{val} #{flags}" } if flags
                      cmd.join(' && ')
                    else
                      [cmd, flags, opts].compact.join(' ')
                    end
            else
              return unless (opts || extra) && respond_to?(:compose)

              cmd = compose(as_get(opts, from), flags, script: true, args: extra, from: from)
              from = :script if from == :run && script?
            end
          end
          run(cmd, var, sync: sync, banner: banner, from: from)
        end

        def depend(*, sync: invoked_sync?('depend'), **)
          run_b(@depend, sync: sync, from: :depend)
        end

        def prereqs(*, sync: invoked_sync?('prereqs'), **)
          on :first, :prereqs
          graph_deps.flatten(1).sort.each do |proj|
            next if @@graph[:_].include?(proj)

            if (val = ENV["PREREQS_#{proj.instance_variable_get(:@envname)}"] || ENV["PREREQS_#{proj.ref.upcase}"])
              split_escape(val) do |meth|
                if proj.respond_to?(meth.to_sym)
                  begin
                    proj.__send__(meth, sync: sync)
                  rescue StandardError => e
                    on_error(e, :prereqs, exception: true)
                  end
                else
                  print_error(name, "method: #{meth}", subject: 'prereqs', hint: 'undefined')
                end
              end
            elsif proj.build?
              proj.build(sync: sync)
            end
            @@graph[:_] << proj
          end
          on :last, :prereqs
        end

        def archive(*, sync: invoked_sync?('archive'), **)
          return unless @archive.is_a?(Hash)

          unpack(path, **@archive, sync: sync, from: :archive)
        end

        def doc(*, sync: invoked_sync?('doc'), **)
          run_b(@doc, sync: sync, banner: from_base?('doc') ? verbose? : !silent?, from: :doc)
        end

        def lint(*, sync: invoked_sync?('lint'), **)
          run_b(@lint, sync: sync, from: :lint)
        end

        def test(*, sync: invoked_sync?('test'), **)
          run_b(@test, sync: sync, from: :test)
        end

        def copy(*, sync: invoked_sync?('copy'), **)
          run_b(@copy, sync: sync, from: :copy)
        end

        def clean(*args, sync: invoked_sync?('clean'), pass: false, **kwargs)
          return unless @clean

          on :first, :clean unless pass
          case @clean
          when Struct
            if (val = instance_eval(&@clean.block) || @clean.run)
              @clean = @clean.tap do
                @clean = val
                clean(*args, sync: sync, pass: true, **kwargs)
              end
            end
          when String
            run_s(@clean, sync: sync)
          when Hash
            begin
              @clean.each { |cmd, opts| build(cmd.to_s, opts, sync: sync) }
            rescue StandardError => e
              on_error e, :clean
            end
          else
            if @clean.is_a?(Enumerable) && !series?(@clean)
              @clean.each do |val|
                entry = basepath(val = val.to_s)
                if entry.directory? && val.match?(%r{[\\/]\z})
                  log&.warn "rm -rf #{entry}"
                  rm_rf(entry, verbose: !silent?)
                else
                  log&.warn "rm #{entry}"
                  (val.include?('*') ? Dir[entry] : [entry]).each do |file|
                    next unless File.file?(file)

                    File.delete(file)
                  rescue StandardError => e
                    log&.error e
                  end
                end
              end
            else
              run_b(@clean, sync: sync)
            end
          end
          on :last, :clean unless pass
        end

        def graph(start = [], tasks = nil, *, sync: invoked_sync?('graph'), pass: [], out: nil, order: nil, **)
          env('GRAPH', strict: true) do |val|
            tasks ||= []
            split_escape(val) do |task|
              if ref?(task.to_sym) && (script = workspace.script_get(:graph, ref: task.to_sym))
                tasks.concat(script[:graph])
              else
                tasks << task
              end
            end
          end
          env('GRAPH', suffix: 'PASS') { |val| pass.concat(split_escape(val)) }
          start, neg = start.partition { |name| !name.start_with?('-') }
          data = graph_collect(self, start, pass: neg.map! { |name| name[1..-1] })
          unless out
            data[name] << self
            on :first, :graph
          end
          ret = graph_branch(self, data, tasks, out, sync: sync, pass: pass, order: order)
        rescue StandardError => e
          on_error(e, :graph, exception: true)
        else
          if out
            if order
              out.map! do |val|
                name = ret.find { |proj| val.match?(/ #{Regexp.escape(proj.name)}(?:@\d|\z)/) }&.name
                next val unless (n = name && order[name])

                val.subhint(n.succ)
              end
            else
              [out, ret]
            end
          else
            on :last, :graph
          end
        end

        def unpack(target, file = nil, uri: nil, sync: true, digest: nil, ext: nil, force: false, depth: 1, headers: {},
                   verbose: !silent?, from: :unpack)
          if !target.exist?
            target.mkpath
          elsif !target.directory?
            raise_error Errno::EEXIST, target, hint: uri
          elsif !file && !target.empty?
            raise_error Errno::EEXIST, target, hint: uri unless force || env('UNPACK_FORCE')
            create = true
          end
          if digest
            require 'digest'
            digest, type = digest.split(':', 2).reverse
            algo = case type&.downcase || digest.size
                   when 32, 'md5'
                     Digest::MD5
                   when 'rmd160'
                     Digest::RMD160
                   when 40, 'sha1'
                     Digest::SHA1
                   when 64, 'sha256'
                     Digest::SHA256
                   when 96, 'sha384'
                     Digest::SHA384
                   when 128, 'sha512'
                     Digest::SHA512
                   else
                     raise_error "invalid checksum: #{digest}", hint: uri
                   end
          end
          env('HEADERS') do |val|
            next unless (data = parse_json(val))

            headers = headers.is_a?(Hash) ? headers.merge(data) : data
          end
          if file
            ext ||= File.extname(file)[1..-1]
          else
            require 'open-uri'
            data = nil
            (uri = Array(uri)).each_with_index do |url, i|
              URI.open(url, headers) do |f|
                data = f.read
                if algo && algo.hexdigest(data) != digest
                  data = nil
                  raise_error "invalid checksum: #{digest}", hint: url if i == uri.size.pred
                end
                next if ext && i == 0

                case f.content_type
                when 'application/zip'
                  ext = 'zip'
                when %r{application/(?:x-)?gzip}
                  ext = 'tgz'
                when 'application/x-xz'
                  ext = 'txz'
                when 'application/x-7z-compressed'
                  ext = '7z'
                end
              end
              break uri = url if data
            end
            unless data && (ext ||= URI.decode_www_form_component(URI.parse(uri).path[/\.([\w%]+)(?:\?|\z)/, 1]))
              raise_error(data ? TypeError : RuntimeError, "no content#{' type' if data}", hint: uri)
            end
          end
          ext = ext.downcase
          if (val = env("#{%w[zip 7z gem].include?(ext) ? ext.upcase : 'TAR'}_DEPTH", ignore: false))
            depth = val.to_i
          end
          begin
            unless file
              file = if ext == 'gem'
                       dir = Dir.mktmpdir
                       File.new(File.join(dir, File.basename(uri)), 'w')
                     else
                       require 'tempfile'
                       Tempfile.new("#{name}-")
                     end
              file.write(data)
              file.close
              file = Pathname.new(file)
              delete = true
            end
            if create
              print_error('force remove', subject: name, hint: target)
              target.rmtree
              target.mkpath
            end
            case ext
            when 'zip', 'aar'
              session 'unzip', shell_quote(file), quote_option('d', target)
            when 'tar', /\A(?:t|tar\.)?[gx]z\z/
              flags = +(silent? ? '' : 'v')
              if ext.end_with?('gz')
                flags += 'z'
              elsif ext.end_with?('xz')
                flags += 'J'
              end
              session 'tar', "-x#{flags}", basic_option('strip-components', depth), quote_option('f', file),
                      quote_option('C', target)
              depth = 0
            when '7z'
              session '7z', 'x', shell_quote(file), "-o#{shell_quote(target)}"
            when 'gem'
              session 'gem', 'unpack', shell_quote(file), quote_option('target', target)
              depth = 0 unless val
            else
              raise_error("unsupported format: #{ext}", hint: uri || file)
            end
            run(sync: sync, banner: verbose, from: from)
            while depth > 0 && target.children.size == 1
              entry = target.children.first
              break unless entry.directory?

              i = 0
              i += 1 while (dest = target + "#{File.basename(file)}-#{i}").exist?
              FileUtils.mv(entry, dest)
              dest.each_child { |child| FileUtils.mv(child, target) }
              dest.rmdir
              target = entry
              depth -= 1
            end
          ensure
            if dir
              remove_entry dir
            elsif delete && file&.exist?
              file.unlink
            end
          end
        end

        def asdf(flag, opts = [], version: nil)
          return unless @asdf

          cmd = flag == :update ? session('asdf', 'plugin update') : session('asdf', flag)
          name = @asdf.first
          legacy = @@asdf.version == 15
          banner = true
          case flag
          when :set
            u = has_value?(opts, 'u', 'home')
            cmd << if legacy
                     cmd.delete(flag)
                     u ? 'global' : 'local'
                   elsif has_value?(opts, 'p', 'parent')
                     '--parent'
                   elsif u
                     '--home'
                   end
            cmd << name << version
          when :exec
            cmd << name unless opts.first.start_with?(/#{name}\b/)
            cmd.merge(opts)
          when :current
            cmd << '--no-header' unless legacy
            cmd << name
          else
            cmd << name
            banner = false if flag == :latest || flag == :where
          end
          success?(run(banner: banner, from: :"asdf:#{flag}"), flag == :set || flag == :reshim)
        end

        def first(key, *args, **kwargs, &blk)
          event(:first, key, *args, **kwargs, &blk)
        end

        def last(key, *args, **kwargs, &blk)
          event(:last, key, *args, **kwargs, &blk)
        end

        def error(key, *args, **kwargs, &blk)
          event(:error, key, *args, **kwargs, &blk)
        end

        def event(name, key, *args, override: false, **kwargs, &blk)
          args.unshift(blk) if block_given?
          ev = @events[name.to_sym]
          (override ? ev[key.to_sym] = [] : ev[key.to_sym] ||= []) << [args, kwargs]
          self
        end

        def as(cmd, script, to = nil)
          data = @as[cmd.to_sym]
          (to ? [[script, to]] : script).each { |key, val| data[key.to_s] = val }
          self
        end

        def series(key, override: false, &blk)
          if blocks.include?(key.to_sym) && block_given?
            if !override && series?(target = instance_variable_get(:"@#{key}"))
              target << blk
            else
              instance_variable_set :"@#{key}", [blk]
            end
          else
            log&.warn "series: @#{key}".subhint('invalid')
          end
          self
        end

        def run(cmd = @session, var = nil, exception: self.exception, sync: true, banner: true, from: nil, chdir: path,
                interactive: nil, hint: nil, series: false, **)
          unless cmd
            print_error('no command session started', subject: project, hint: from, pass: true)
            return
          end
          cmd = cmd.target if cmd.is_a?(OptionPartition)
          if interactive && sync && (!@session || !option('y'))
            msg, y, h = case interactive
                        when Array
                          interactive
                        when String
                          [interactive, 'N']
                        else
                          %w[Run Y]
                        end
            msg = "#{msg} #{sub_style(h, theme[:active])}" if h
            exit 1 unless confirm_basic("#{msg}?", cmd, y)
          end
          cmd = session_done cmd
          log&.info cmd
          on :first, from
          begin
            if cmd.start_with?(/[^:]+:[^:]/) && workspace.task_defined?(cmd)
              log&.warn "ENV discarded: #{var}" if var
              task_invoke(cmd, exception: exception, warning: warning?)
            else
              print_item(format_banner(cmd, banner: banner, hint: hint), series: series) if sync
              if var != false && (pre = runenv)
                case pre
                when Hash
                  var = var.is_a?(Hash) ? pre.merge(var) : pre
                when Enumerable
                  cmd = command(*pre.to_a, cmd)
                else
                  cmd = command pre, cmd
                end
              end
              args = var.is_a?(Hash) ? [var, cmd] : [cmd]
              ret = shell(*args, chdir: chdir, exception: exception)
            end
          rescue StandardError => e
            on_error(e, from, exception: true)
            false
          else
            on :last, from
            ret
          end
        end

        def scope(*args, **kwargs, &blk)
          namespace name do
            task(*args, **kwargs, &blk)
          end
        end

        def variable_set(key, *args, **kwargs, &blk)
          if block_given?
            if blocks.include?(key)
              series key, &blk
              return self
            end
            args = block_args args, &blk
          end
          if variables.include?(key) || blocks.include?(key)
            val = args.size > 1 ? args : args.first
            case key
            when :index
              index_set val
            when :graph
              graph_set val
            when :pass
              pass_set val
            when :only
              only_set val
            when :exclude
              exclude_set val
            when :parent
              parent_set val
            when :archive
              archive_set val
            when :asdf
              asdf_set val
            when :run
              run_set(*args, **kwargs)
            when :script
              script_set(*args, **kwargs)
            when :env
              run_set(output[0], *args, **kwargs)
            when :dependfile
              @dependindex = nil
              @dependfile = val.nil? ? nil : basepath(*args)
            else
              instance_variable_set(:"@#{key}", val)
            end
          else
            log&.warn "variable_set: @#{key}".subhint('private')
          end
          self
        end

        alias apply variable_set

        def enabled?(ref = nil, **)
          return false if ref && !ref?(ref)

          (path.directory? && !path.empty?) || archive?
        end

        def has?(meth, ref = nil)
          return false if ref && !ref?(ref)

          respond_to?(meth = :"#{meth}?") && __send__(meth)
        end

        def ref?(val)
          @ref.include?(val)
        end

        def exist?(*args)
          return false if (args = args.compact).empty?

          basepath(*args).exist?
        end

        def build?
          !!@output[0] || script? || series?(@run)
        end

        def script?
          @output[0].nil? && !!@output[1] && respond_to?(:compose)
        end

        def depend?
          !!@depend
        end

        def archive?
          @archive.is_a?(Hash) && (!path.exist? || path.empty?)
        end

        def graph?
          @graph.is_a?(Array) && !@graph.empty?
        end

        def prereqs?
          target = self
          loop do
            return true if target.graph?
            break unless (target = target.parent)
          end
          false
        end

        def copy?
          runnable?(@copy) || workspace.task_defined?(name, 'copy')
        end

        def doc?
          !!@doc
        end

        def lint?
          !!@lint
        end

        def test?
          !!@test
        end

        def clean?
          runnable?(@clean) || workspace.task_defined?(name, 'clean')
        end

        def dev?
          @dev != false && workspace.dev?(pat: @dev, **scriptargs)
        end

        def prod?
          @prod != false && workspace.prod?(pat: @prod, **scriptargs)
        end

        def empty?
          children.empty?
        end

        def exclude?(*refs)
          !@exclude.empty? && has_value?(@exclude, refs.flatten)
        end

        def task_include?(key, ref = nil)
          workspace.task_include?(self, key, ref) && !@pass.include?(key.to_s)
        end

        def version(*)
          @version
        end

        def dependtype(*)
          @dependindex ? @dependindex.succ : 0
        end

        def dependname
          @dependname ||= dependfile&.basename.to_s
        end

        def log
          return @log unless @log.is_a?(Array)

          @log = Logger.new((@log.first if enabled?), **@log.last)
        end

        def allref(&blk)
          @ref.reverse_each(&blk)
        end

        def basepath(*args)
          path.join(*args)
        end

        def basepath!(*args, type: nil)
          ret = basepath(*args)
          return unless ret.exist?

          if type
            (type.is_a?(String) ? type.chars : type).each do |ch|
              case ch
              when 'f'
                return nil unless ret.file?
              when 'd'
                return nil unless ret.directory?
              when 'l'
                return nil unless ret.symlink?
              when 'r'
                return nil unless ret.readable?
              when 'w'
                return nil unless ret.writable?
              when 'e'
                return nil unless ret.executable?
              else
                return nil
              end
            end
          end
          ret
        end

        def rootpath(*args, ascend: nil)
          ret = basepath(*args)
          return ret unless ascend && !ret.exist?

          path.parent.ascend.each do |dir|
            target = dir.join(*args)
            return target if target.exist?
            break if (ascend.is_a?(String) && dir.join(ascend).exist?) || workspace.root == dir || parent&.path == dir
          end
          ret
        end

        def localname
          workspace.task_localname(name)
        end

        def scriptname(from: :run)
          return unless (name = @output[1]) && respond_to?(:compose)

          as_get name, from
        end

        def inspect
          "#<#{self.class}: #{name} => #{self}>"
        end

        def to_s
          path.to_s
        end

        def to_sym
          name.to_sym
        end

        protected

        def script_get(*args, key: nil)
          ret = workspace.script_get(*args, group: group, ref: allref)
          return ret unless ret && key

          ret.fetch(key, nil)
        end

        private

        def puts(*args, **kwargs)
          log_console(*args, pipe: kwargs[:pipe] || pipe)
        end

        def run_s(*cmd, sync: true, banner: !silent?, from: nil, **kwargs)
          cmd.flatten!
          case cmd.last
          when Hash, TrueClass, FalseClass
            var = cmd.pop
          end
          on :first, from
          begin
            cmd.each do |val|
              print_run val, banner
              run(val, var, sync: sync, banner: banner, **kwargs)
            end
          rescue StandardError => e
            on_error(e, from, exception: kwargs.fetch(:exception, exception))
          end
          on :last, from
        end

        def run_b(obj, **kwargs)
          case obj
          when Struct
            if (val = instance_eval(&obj.block) || obj.run)
              run_b(val, **kwargs)
            end
          when Proc
            instance_eval(&obj)
          when Method
            args = if (n = obj.arity.abs) > 0
                     Array.new(n).tap { |data| data[0] = self }
                   else
                     []
                   end
            obj.call(*args)
          else
            if series?(obj)
              obj.each(&:call)
            elsif obj.is_a?(Array) && obj.any? { |val| !val.is_a?(String) }
              build(*obj, **kwargs)
            elsif obj
              run_s(*Array(obj), **kwargs)
            end
          end
        end

        def graph_branch(target, data, tasks = nil, out = nil, sync: true, pass: [], done: [], order: nil, depth: 0,
                         single: false, last: false, context: nil)
          tag = ->(proj) { "#{proj.name}#{"@#{proj.version}" if SEM_VER.match?(proj.version)}" }
          uniq = lambda do |name|
            return [] unless (ret = data[name])

            ret.dup.each do |proj|
              next if proj.name == name

              data[proj.name]&.each { |dep| ret.delete(dep) if ret.include?(dep) }
            end
            ret
          end
          if depth == 0
            items = uniq.call(target.name) - done
            single = items.size == 1
          else
            items = data[target.name] - done
          end
          return done if items.empty?

          if out
            a, b, c, d, e = ARG[:GRAPH]
            f = tag.call(target)
            out << case depth
                   when 0
                     f
                   when 1
                     if items.empty?
                       "#{d}#{b * 4} #{f}"
                     else
                       "#{last ? d : c}#{b * 3}#{e} #{f}"
                     end
                   else
                     "#{single ? ' ' : a}#{'   ' * depth.pred}#{last ? d : c}#{b * 3}#{items.empty? ? b : e} #{f}"
                   end
          end
          items.each_with_index do |proj, i|
            next if done.include?(proj)

            t = uniq.call(name = proj.name)
            j = if out
                  if i == items.size.pred || (post = items[i.succ..-1] - done).empty?
                    true
                  elsif !t.empty? && depth > 0
                    (post - t).empty?
                  end
                end
            unless target.name == name || (none = (t - done).empty?)
              graph_branch(proj, data, tasks, out, sync: sync, pass: pass, done: done, order: order, depth: depth.succ,
                                                   single: single, last: j == true, context: target)
            end
            if out
              if none
                a, b, c, d = ARG[:GRAPH]
                out << if depth == 0
                         "#{i == items.size.pred ? d : c}#{b * 4} #{tag.call(proj)}"
                       else
                         s = +''
                         k = 0
                         final = data.keys.last
                         while k < depth
                           indent = k > 0 ? ((last && !j) || (j && k == depth.pred) || single) : j && last && depth == 1
                           s += "#{indent || (last && data[final].last == context) ? ' ' : a}   "
                           k += 1
                         end
                         s += "#{j ? d : c}#{b * 3} #{tag.call(proj)}"
                       end
              end
              if order
                n = order.size
                order[name] ||= if proj.parent
                                  if order[s = proj.parent.name]
                                    order[s] += 1
                                    n.pred
                                  else
                                    order[s] = n.succ
                                    n
                                  end
                                else
                                  n
                                end
              end
            else
              (tasks || (graph = proj.script_get(:graph, key: :graph)) || (dev? ? %w[build copy] : %w[depend build]))
                .each do |meth|
                  next if pass.include?(meth)

                  if workspace.task_defined?(cmd = task_join(name, meth))
                    if ENV.key?(key = "BANNER_#{name.upcase}")
                      key = nil
                    else
                      ENV[key] = '0'
                    end
                    run(cmd, sync: false, banner: false)
                    ENV.delete(key) if key
                  elsif proj.has?(meth, (workspace.baseref unless tasks || graph))
                    proj.__send__(meth.to_sym, sync: sync)
                  end
                end
            end
            done << proj
          end
          done
        end

        def graph_collect(target, start = [], data: {}, pass: [], root: [])
          deps = []
          (start.empty? ? target.instance_variable_get(:@graph) : start)&.each do |val|
            next if pass.include?(val)

            if (obj = workspace.find(name: val))
              obj.enabled? ? [obj] : []
            else
              workspace.find(group: val, ref: val.to_sym).sort
            end.each do |proj|
              next if pass.include?(name = proj.name)

              if proj.graph? && !data.key?(name) && !root.include?(name)
                graph_collect(proj, data: data, pass: pass, root: root + [name, target.name])
              end
              next if (objs = data.fetch(name, [])).include?(target)

              deps << proj
              deps.concat(objs)
            end
          end
          deps.uniq!
          deps.delete(target)
          data[target.name] = deps
          data
        end

        def graph_deps(target = self)
          key = target.name
          return @@graph[key] if @@graph.key?(key)

          base = []
          deps = []
          loop do
            deps.concat(graph_branch(target, graph_collect(target), []))
            break unless (target = target.parent)

            base << target
          end
          deps.uniq!
          @@graph[key] = [base, deps]
        end

        def env(key, default = nil, suffix: nil, equals: nil, ignore: nil, strict: false)
          name = "#{key}_#{@envname}"
          ret = if suffix
                  ENV.fetch("#{name}_#{suffix}", '')
                elsif strict
                  ENV[name].to_s
                else
                  ignore = ['0'].freeze if ignore.nil?
                  ENV[name] || ENV.fetch(key, '')
                end
          if equals.nil?
            ret = default if ret.empty? || (ignore && Array(ignore).any? { |val| ret == val.to_s })
            return ret if ret.nil?
          else
            ret = Array(equals).any? { |val| ret == val.to_s }
          end
          return yield ret if block_given?

          ret
        end

        def session(*cmd, prefix: cmd.first, main: true, path: true, options: true)
          prefix = prefix.to_s.stripext
          if path && (val = shell_bin(prefix))
            cmd[0] = shell_quote(val, force: false)
          end
          ret = JoinSet.new(cmd.flatten(1))
          if options
            env("#{prefix.upcase}_OPTIONS") do |val|
              if val.start_with?('-')
                ret.merge(shell_parse(val))
              else
                split_escape(val) { |opt| ret.last(fill_option(opt), /\A(--?[^=\s-][^=\s]*)[=\s].+\z/m) }
              end
            end
          end
          return ret unless main

          @session = ret
        end

        def session_output(*cmd, **kwargs)
          session(*cmd, main: false, options: false, **kwargs)
        end

        def session_get(val, pass: nil)
          base = OPTIONS[ref]
          args = []
          kwargs = {}
          with = []
          [:_, name.to_sym].each do |key|
            next unless base.key?(key) && (a, b, c = base[key][val])

            args.concat(a)
            append_keys(kwargs, b, :opts)
            with.concat(c)
          end
          OptionPartition.uniq!(args, pass) if pass
          [args, kwargs, with]
        end

        def session_apply(val, args: nil, kwargs: nil, pass: [], keys: [:opts], exclude: [])
          a = []
          b = {}
          Array(val).each do |c|
            d, e, f = session_get c
            unless (f -= exclude).empty?
              h = []
              i = {}
              session_apply(f.map!(&:to_s), args: h, kwargs: i, pass: pass, keys: keys, exclude: exclude.concat(f))
              a.concat(h)
              append_keys(b, i, *keys)
            end
            a.concat(d)
            append_keys(b, e, *keys)
          end
          if args
            args.unshift(*a)
            OptionPartition.uniq!(args, pass) if pass
          end
          kwargs&.update(b) { |_, val| val }
          nil
        end

        def session_opts(val, args: nil, kwargs: nil, pass: nil, keys: [:opts])
          opts = kwargs.delete(:opts) || []
          return opts unless val

          session_apply(val, args: args, kwargs: kwargs, pass: pass, keys: keys)
          kwargs.fetch(:opts, []).concat(opts)
        end

        def session_done(cmd)
          return cmd.to_s unless cmd.respond_to?(:done)

          raise_error 'no command added', hint: cmd.first unless cmd.size > 1
          @session = nil if cmd == @session
          cmd.done
        end

        def session_arg?(*args, target: @session, **kwargs)
          return false unless target

          OptionPartition.arg?(target, *args, **kwargs)
        end

        def option(*args, target: @session, prefix: target&.first, **kwargs)
          return unless prefix

          args.each do |val|
            next unless (ret = env(env_key(prefix.to_s.stripext, val), **kwargs))

            return block_given? ? yield(ret) : ret
          end
          nil
        end

        def option_clear(opts, empty = true, target: @session, **kwargs)
          return unless target

          OptionPartition.clear(target, opts, styles: theme[:inline], **kwargs)
          opts.clear if empty
          nil
        end

        def print_success
          puts 'Success'
        end

        def print_error(*args, loglevel: Logger::WARN, **kwargs)
          return unless warning?

          warn log_message(loglevel, *args, **kwargs)
        end

        def print_run(cmd, banner = true, verbose: nil, **)
          return if banner || !stdout? || verbose == false || env('BANNER', equals: '0')

          puts "\n> #{cmd}"
          printsucc
        end

        def print_item(*val, series: false)
          puts unless printfirst?
          printsucc unless series
          puts val unless val.empty? || (val.size == 1 && !val.first)
        end

        def print_banner(*lines, client: false, styles: theme[:banner], border: borderstyle, **)
          pad = 0
          if styles
            if styles.any? { |s| s.to_s.end_with?('!') }
              pad = 1
            elsif !client && styles.size <= 1
              styles = [:bold] + styles
            end
          end
          n = line_width lines
          ch = ' ' * pad
          index = -1
          lines.map! do |val|
            index += 1
            val = ch + val.ljust(n - (pad * 2)) + ch
            if styles && (pad == 1 || index == 0)
              sub_style(val, *styles)
            else
              val
            end
          end
          (lines << sub_style(ARG[:BORDER][1] * n, border)).join("\n")
        end

        def print_footer(*lines, sub: nil, reverse: false, right: false, border: borderstyle, **)
          n = line_width lines
          sub = as_a sub
          lines.map! do |val|
            s = right ? val.rjust(n) : val.ljust(n)
            sub.each { |h| sub_style!(s, **h) }
            s
          end
          [sub_style(ARG[:BORDER][1] * n, border)].concat(lines)
                                                  .tap { |ret| ret.reverse! if reverse }
                                                  .join("\n")
        end

        def print_status(*args, from: nil, **kwargs)
          return if stdin? || silent?

          case from
          when :outdated
            out = print_footer("major #{args[0]} / minor #{args[1]} / patch #{args[2]}", right: true).split("\n")
            sub_style!(out[1], **opt_style(theme[:major], /^( +major )(\d+)(.+)$/, 2))
            sub_style!(out[1], **opt_style(theme[:active], /^(.+)(minor )(\d+)(.+)$/, 3))
            sub_style!(out[1], **opt_style(theme[:current], /^(.+)(patch )(\d+)(.+)$/, 3)) if theme[:current]
          when :completed
            return unless kwargs[:start]

            out = log_message(Logger::INFO, *args, sub_style('completed', theme[:active]),
                              subject: kwargs[:subject], hint: time_format(time_epoch - kwargs[:start]))
          else
            return
          end
          puts out
        end

        def format_desc(action, flag, opts = nil, **kwargs)
          return unless TASK_METADATA

          workspace.format_desc([@desc, action, flag].compact, opts, **kwargs)
        end

        def format_banner(cmd, banner: true, hint: nil, strip: nil)
          return unless banner && banner?

          if (data = workspace.banner_get(*@ref, group: group))
            return if !data.command && data.order.empty?

            client = true
          else
            data = Struct::BannerData.new(true, [:path], theme[:banner], theme[:border])
          end
          if verbose
            out = []
            if data.command
              if cmd =~ /\A(?:"((?:[^"]|(?<=\\)")+)"|'((?:[^']|(?<=\\)')+)'|(\S+))( |\z)/
                arg = $3 || $2 || $1
                cmd = cmd.sub(arg, data.command == 0 ? arg.stripext : arg.stripext.upcase)
              end
              if strip || (strip.nil? && data.order.include?(:path))
                cmd = cmd.gsub(/(?:#{s = Regexp.escape(File.join(path, ''))}?(?=["'])|#{s})/, '')
                         .gsub(/(?: -[^ ])? (?:""|'')/, '')
              end
              out << cmd.subhint(hint)
            end
            data.order.each do |val|
              if val.is_a?(Array)
                s = ' '
                found = false
                val = val.map do |meth|
                  if meth.is_a?(String)
                    s = ''
                    meth
                  elsif respond_to?(meth)
                    found = true
                    __send__ meth
                  end
                end
                val = val.compact.join(s)
                next unless found && !val.empty?
              elsif (val = __send__(val)).nil?
                next
              end
              out << val.to_s
            end
            print_banner(*out, styles: data.styles, border: data.border, client: client)
          elsif workspace.series.multiple?
            "## #{__send__(data.order.first || :path)} ##"
          end
        end

        def format_list(items, cmd, type, grep: [], from: nil)
          reg = grep.map { |val| Regexp.new(val) }
          out = []
          unless items.empty?
            pad = items.size.to_s.size
            items.each_with_index do |val, i|
              next unless matchany?(val.first, reg)

              out << ('%*d. %s' % [pad, i.succ, block_given? ? yield(val) : val.first])
            end
          end
          sub = [headerstyle]
          pat = if out.empty?
                  out = ["No #{type} were found:", '']
                  unless grep.empty?
                    i = 0
                    out.concat(grep.map { |s| "#{i += 1}. #{s}" })
                    out << ''
                  end
                  if from
                    out << (from = from.to_s)
                    /\A(#{Regexp.escape(from)})(.*)\z/
                  end
                else
                  unless grep.empty?
                    footer = "#{out.size} found "
                    sub << opt_style(theme[:inline], /\A(\d+)( .+)\z/)
                  end
                  /\A(\s*\d+\.)(.+)\z/
                end
          sub << opt_style(theme[:active], pat) if pat
          emphasize(out, title: task_join(name, cmd), border: borderstyle, sub: sub, footer: footer, right: true)
        end

        def empty_status(msg, title, obj, always: false)
          return msg if !always && (!obj || obj == 0 || obj.to_s.empty?)

          msg.subhint(obj.is_a?(Numeric) ? "#{obj} #{title}" : message(title, obj.to_s))
        end

        def append_repeat(flag, opts, target: @session, **kwargs)
          opts.each { |val| target << shell_option(flag, val, **kwargs) }
        end

        def append_hash(data, target: @session || [], build: false)
          if build && (type = env('BUILD', suffix: 'TYPE') || ENV['BUILD_TYPE'])
            if (extra = data[type = "__#{type}__"] || data[type.to_sym]).is_a?(Hash)
              data = data.merge(extra)
            else
              extra = nil
            end
          end
          data.each do |key, val|
            next if (key = key.to_s).start_with?('__')

            if val.nil? || extra || session_arg?(key, target: target)
              OptionPartition.delete_key(target, key)
              next if val.nil?
            end
            case val
            when Hash
              append_hash(val, target: target, build: build)
            when Enumerable
              append_repeat(key, val, target: target)
            when Numeric
              target << basic_option(key, val)
            when FalseClass
              target << shell_option(key).sub(/^--(?!no-)/, '--no-')
            when Pathname
              target << shell_option(key, val, escape: false)
            else
              target << shell_option(key, (val if val.is_a?(String)))
            end
          end
          target
        end

        def append_any(val, target: @session, build: false, delim: false)
          if delim && !target.include?('--')
            target << '--'
          else
            delim = false
          end
          val = shell_split val if val.is_a?(String)
          case val
          when Hash
            append_hash(val, target: target, build: build)
          when Enumerable
            merge_list target, val
          else
            target.delete('--') if delim
            nil
          end
        end

        def append_value(*list, target: @session, **kwargs)
          return unless target

          OptionPartition.append(target, *list, **kwargs)
        end

        def append_first(*list, target: @session, flag: true, equals: false, escape: true, quote: true, force: true,
                         **kwargs)
          return if list.empty?

          list.flatten.each do |opt|
            next unless (val = option(opt, **kwargs))

            return target << if flag
                               shell_option(opt, (val if equals), quote: quote, escape: escape, force: force)
                             else
                               shell_quote val
                             end
          end
          nil
        end

        def append_option(*list, target: @session, no: false, equals: false, escape: true, quote: true, force: true,
                          **kwargs)
          return if list.empty?

          kwargs[:ignore] = false if no && !kwargs.key?(:ignore)
          ret = []
          list.flatten.each do |flag|
            next unless (val = option(flag, target: target, **kwargs))

            if no && val == '0'
              flag = "no-#{flag}"
              val = nil
            end
            ret << shell_option(flag, (val if equals), escape: escape, quote: quote, force: force)
          end
          merge_list target, ret unless ret.empty?
          ret
        end

        def append_nocolor(target: @session)
          target << '--no-color' if !ARG[:COLOR] || stdin? || option('color', target: target, equals: '0')
        end

        def append_keys(base, data, *keys)
          out = {}
          keys.each do |key|
            next unless data.key?(key)

            out[key] = case (val = data[key])
                       when Hash
                         base.fetch(key, {}).update(val)
                       when Enumerable
                         Array(base.fetch(key, [])) + val.to_a
                       else
                         val
                       end
          end
          base.update(data)
              .update(out)
        end

        def merge_opts(base, data)
          case data
          when String
            case base
            when String
              "#{base} #{data}"
            when Hash
              "#{append_hash(base, target: []).join(' ')} #{data}"
            when Enumerable
              "#{base.to_a.join(' ')} #{data}"
            else
              data
            end
          when Hash
            case base
            when String
              "#{base} #{append_hash(data, target: []).join(' ')}"
            when Hash
              base.merge(data)
            when Enumerable
              Set.new(base.to_a + append_hash(data, target: [])).to_a
            else
              data
            end
          when Enumerable
            case base
            when String
              "#{base} #{data.to_a.join(' ')}"
            when Hash
              "#{append_hash(base, target: []).join(' ')} #{data.to_a.join(' ')}"
            when Enumerable
              Set.new(base.to_a + data.to_a).to_a
            else
              data
            end
          else
            base
          end
        end

        def merge_list(base, data)
          data = Array(data)
          case base
          when Array
            base.concat(data)
          when Set
            base.merge(data)
          else
            Array(base).concat(data)
          end
        end

        def collect_hash(data, pass: [])
          ret = []
          data.each { |key, val| ret.concat(val) unless pass.include?(key) }
          ret
        end

        def replace_bin(val)
          a, b = val.split(' ', 2)
          return val if val.start_with?(/["']/) || a.include?(File::Separator)

          [shell_bin(a), b].compact.join(' ')
        end

        def parse_json(val, kind: Hash, hint: nil)
          ret = JSON.parse(val)
          raise_error 'invalid JSON'.subhint(kind.name), val, hint: hint if kind && !ret.is_a?(kind)
          ret
        rescue StandardError => e
          log&.warn e
          print_error(e, subject: name)
        end

        def parse_env(key)
          env(key) do |val|
            val.start_with?('-') ? shell_parse(val) : split_escape(val).map! { |opt| fill_option(opt) }
          end || []
        end

        def param_guard(action, flag, args:, key: nil, pat: nil, values: nil)
          if args && key
            val = args.fetch(key, nil)
            return val unless val.nil? || (pat && !val.match?(pat)) || (values && !values.include?(val))

            @session = nil
            raise_error(action, "#{flag}[#{key}]", hint: val.nil? ? 'missing' : 'invalid')
          elsif args.is_a?(Array) && args.empty?
            @session = nil
            raise_error action, "#{flag}+", hint: 'empty'
          end
          args
        end

        def confirm_basic(msg, hint, default = 'Y', style: :inline, target: @session, prefix: nil, **kwargs)
          return true if prefix ? option('y', prefix: prefix) : target && option('y', target: target)

          confirm("#{msg} [#{sub_style(hint.to_s, style.is_a?(Symbol) ? theme[style] : style)}]", default, **kwargs)
        end

        def confirm_outdated(pkg, ver, type, cur = nil, lock: false, col0: 0, col1: 0, col2: nil, col3: 0, col4: 0,
                             **kwargs)
          h = sub_style(semrev(type).upcase, (type == 1 && theme[:major]) || theme[:header])
          case col0
          when 0
            col0 = "#{h}: "
          when Numeric
            puts h
            col0 = ' ' * col0
          else
            puts h
          end
          b = sub_style pkg.ljust(col1), theme[:inline]
          cur ||= 'locked' if lock
          c = if cur
                cur = cur.ljust(col2 || cur.size.succ)
                lock ? sub_style(cur, color(:red)) : cur
              end
          d = type == 1 || lock ? 'N' : 'Y'
          e = "#{col0}#{b}#{c}#{sub_style(col1 > 0 ? ver.ljust(col3) : ver.rjust(ver.size.succ), theme[:inline])}"
          confirm("#{e}#{col4 > 0 ? ' ' * [col4 - e.stripstyle.size - 1, 2].max : '  '}", d, **kwargs)
        end

        def confirm_semver(msg, type, style: (type == 1 && theme[:major]) || :inline, timeout: 0, **kwargs)
          confirm_basic(msg, semrev(type), type == 1 ? 'N' : 'Y', style: style, timeout: timeout, **kwargs)
        end

        def choice_index(msg, list, values: nil, accept: nil, series: false, trim: nil, column: nil, multiple: false,
                         force: true, **kwargs)
          puts unless series || printfirst?
          ret = choice(msg, list, multiple: multiple, force: force, **kwargs).tap do |val|
            next unless !val || val.empty?

            exit 1 if force
            return nil
          end
          ret = multiple ? ret.map! { |val| val.sub(trim, '') } : ret.sub(trim, '') if trim
          if column
            a, b = Array(column)
            ret = Array(ret).map! { |val| val[a, b || 1] }
            ret = ret.first unless multiple
          end
          if accept
            hint = Array(ret).map { |val| sub_style(val.to_s, theme[:inline]) }.join(', ')
            accept = Array(accept).map { |val| Array(val) }
            ret = Array(ret) if accept.any? { |val| val[1] == true }
            loop do
              item = accept.first
              c = confirm("#{item[0]}#{" [#{hint}]" if hint}", item[2] ? 'Y' : 'N')
              if item[1] == true
                ret << c
              elsif !c
                break
              end
              hint = nil
              accept.shift
              break if accept.empty?
            end
            exit 1 unless accept.empty?
          end
          if values
            ret = Array(ret)
            Array(values).each do |val|
              if val.is_a?(Array)
                val, force = val
              else
                force = false
              end
              val = readline(val, force: force)
              ret << (val unless val.empty?)
            end
          end
          printsucc unless series
          ret
        end

        def accept_b(val, yes = false)
          [val, true, yes]
        end

        def accept_y(val, bool = false)
          [val, bool, true]
        end

        def command_args(args, min: 0, force: false, **kwargs)
          return if args.size > min || option('i', 'interactive', **kwargs, equals: '0')

          readline('Enter arguments', force: force)
        end

        def block_args(fallback = nil, &blk)
          return fallback if (ret = instance_eval(&blk)).nil?

          Array(ret)
        end

        def runenv
          nil
        end

        def command(*args, verbose: true)
          out = unless verbose
                  cmd = [">#{File::NULL}", '2>&1']
                  cmd.reverse! if File::NULL == 'NUL'
                  cmd.unshift('').join(' ')
                end
          if workspace.powershell?
            "#{shell_bin('powershell.exe')} -Command \"& {#{args.join(' ; ')}}\"#{out}"
          else
            args.map! { |val| "#{val}#{out}" }.join(' && ')
          end
        end

        def relativepath(*list, all: false)
          list.flatten.map! { |val| Pathname.new(val) }.select { |val| projectpath?(val) }.map! do |val|
            ret = (val.absolute? ? val.relative_path_from(path) : val.cleanpath).to_s
            all && val.to_s.end_with?('/') ? "#{ret}/*" : ret
          end
        end

        def projectmap(files, parent: false, pass: true)
          unless parent
            proj = files.select { |val| projectpath?(val) }
            raise_error 'pathspec not within worktree' unless pass || files.size == proj.size
            files = proj
          end
          files.map { |val| val == '.' ? '.' : shell_quote(basepath(val)) }
        end

        def matchmap(list, prefix = nil)
          list.map do |val|
            next val if val.is_a?(Regexp)

            val = ".*#{val}" if prefix && !val.sub!(/\A(\^|\\A)/, '')
            Regexp.new("#{prefix}#{val == '*' ? '.+' : val}")
          end
        end

        def semver(val)
          return val if val[3]

          val[3] = '.'
          val[4] = '0'
          unless val[1]
            val[1] = '.'
            val[2] = '0'
          end
          val
        end

        def semscan(val, fill: true)
          ret = val.scan(SEM_VER).first
          fill ? semver(ret) : ret
        end

        def semcmp(val, other)
          return 0 if val == other
          return -1 if (b = other.scan(SEM_VER)).empty?
          return 1 if (a = val.scan(SEM_VER)).empty?

          a, b = [a.first, b.first].map! do |c|
            d = begin
              Integer(c[5]).to_s
            rescue StandardError
              c[5] ? '-1' : '0'
            end
            [c[0], c[2], c[4] || '0', d]
          end
          a.each_with_index do |c, i|
            next if c == (d = b[i])

            return c.to_i < d.to_i ? 1 : -1
          end
          0
        end

        def sembump(val, flag = :patch, join: true)
          ret = semscan(val, fill: false)
          case flag
          when :major
            ret[2] = if ret[0] != '0' || ret[2].nil?
                       ret[0] = ret[0].succ
                       '0'
                     else
                       ret[2].succ
                     end
            ret[4] = '0'
          when :minor
            if ret[0] == '0'
              ret[4] &&= ret[4].succ
            else
              ret[2] = ret[2].succ
              ret[4] &&= '0'
            end
          when :patch
            ret[4] &&= ret[4].succ
          end
          join ? ret.join : ret
        end

        def semtype(cur, lat)
          if semmajor?(cur, lat)
            1
          else
            cur[2] == lat[2] ? 3 : 2
          end
        end

        def semrev(type)
          case type
          when 1
            'major'
          when 2
            'minor'
          when 3
            'patch'
          else
            'unknown'
          end
        end

        def semgte?(val, other = nil)
          unless other
            other = val
            val = RUBY_VERSION
          end
          semcmp(val, other) != 1
        end

        def indexitem(val)
          [$1.to_i, $2 && $2[1..-1]] if val =~ /\A[=^#{indexchar}](\d+)(:.+)?\z/
        end

        def indexerror(val, list = nil)
          raise_error IndexError, "requested index #{val}", hint: ("of #{list.size}" if list)
        end

        def indexchar
          workspace.windows? ? '=' : '^'
        end

        def shortname(*args, suffix: '?', delim: ',', pass: false)
          return unless TASK_METADATA || pass

          args.map! do |ch|
            "#{ch}/#{case ch
                     when 'i'
                       'nteractive'
                     when 's'
                       'elect'
                     when 'u'
                       'pdate'
                     when 'h'
                       'ide'
                     when 'f'
                       'orce'
                     when 'd'
                       'ry-run'
                     else
                       next
                     end}#{suffix}"
          end.compact
             .join(delim)
        end

        def printsucc
          @@print_order += 1
        end

        def color(val)
          (ret = theme[val]) && !ret.empty? ? ret : [val]
        end

        def colormap(val)
          val.compact.flat_map { |s| color(s) }
        end

        def verbosetype
          case verbose
          when TrueClass
            1
          when Numeric
            verbose.succ
          else
            0
          end
        end

        def on(event, from, *args, **kwargs)
          return unless from && @events.key?(event)

          Array(@events[event][from]).each do |obj|
            target, opts = if obj.is_a?(Array) && obj[1].is_a?(Hash)
                             [obj[0], kwargs.empty? ? obj[1] : obj[1].merge(kwargs)]
                           else
                             [obj, kwargs]
                           end
            as_a(target, flat: true).each do |cmd|
              case cmd
              when Proc, Method
                cmd.call(*args, **opts)
              when String
                run(cmd, **opts)
              end
            end
          end
        end

        def on_error(err, from, pass: false, exception: self.exception, dryrun: false)
          log&.error err
          unless dryrun
            ret = on :error, from, err
            raise err if exception && ret != true
          end
          print_error(err, pass: pass) unless ret
        end

        def pwd_set(pass: false, exception: self.exception, dryrun: false, from: nil)
          return yield if (path.to_s == Dir.pwd || pass == true) && (workspace.mri? || !workspace.windows?)

          pwd = Dir.pwd
          Dir.chdir(path)
          yield
        rescue StandardError => e
          on_error(e, from, exception: exception, dryrun: dryrun)
        ensure
          Dir.chdir(pwd) if pwd
        end

        def run_set(cmd, val = nil, opts: nil, global: false, **)
          noopt = @output[1] == false && !@output[0].nil?
          noenv = @output[2] == false
          parse = lambda do |data|
            ret = []
            if data[:command]
              ret[0] = data[:command]
              ret[1] = data[:opts] unless diso
              ret[3] = data[:args]
            elsif data[:script]
              ret[1] = data[:script]
              ret[3] = data[:opts]
              ret[4] = data[:args]
            else
              ret[0] = false
            end
            ret[2] = data[:env] unless dise
            ret
          end
          self.global = global
          case cmd
          when Hash
            @output = parse.call(data)
          when Enumerable
            @output = if cmd.all? { |data| data.is_a?(Hash) }
                        noopt = false
                        noenv = false
                        cmd.map { |data| parse.call(data) }
                      else
                        cmd.dup
                      end
            return
          else
            @output[0] = cmd
          end
          unless noopt
            if opts == false
              @output[1] = false
            elsif opts && opts != true
              @output[1] = opts
            end
          end
          return if noenv

          if val.is_a?(Hash)
            @output[2] = val
          elsif val == false
            @output[2] = false
          end
        end

        def script_set(cmd, prod: nil, args: nil, global: false, **)
          return if @output[1] == false && @output[0].nil?

          self.global = global
          @output[0] = nil
          @output[1] = if self.global && cmd.is_a?(Array)
                         cmd[prod == true ? 1 : 0]
                       else
                         cmd
                       end
          @output[4] = args unless @output[4] == false || args.nil?
        end

        def index_set(val)
          @index = val if val.is_a?(Numeric)
        end

        def parent_set(val)
          @parent = val if val.is_a?(Project::Base)
        end

        def graph_set(val)
          @graph = if val
                     Array(val).map { |s| workspace.prefix ? workspace.task_name(s).to_sym : s.to_sym }.freeze
                   end
        end

        def pass_set(val)
          @pass = Array(val).freeze
        end

        def only_set(val)
          @only = val && as_a(val, :to_s).freeze
        end

        def exclude_set(val)
          @exclude = (val ? as_a(val, :to_sym) : []).freeze
        end

        def archive_set(val)
          @archive = case val
                     when String, Array
                       { uri: val }
                     when Hash
                       val
                     end
        end

        def asdf_set(val)
          @asdf = if @@asdf && val
                    dir = @@asdf.path.join('installs', val)
                    [val, dir] if dir.directory? && !dir.empty?
                  end
        end

        def theme_set(common)
          @theme = if silent?
                     {}
                   elsif common
                     workspace.theme
                   else
                     __get__(:theme)[:project][to_sym] ||= {}
                   end
        end

        def dependfile_set(list)
          @dependindex = if @dependname
                           @dependfile = basepath @dependname
                           list.index(@dependname)
                         else
                           list.index { |file| exist?(file) }.tap { |i: 0| @dependfile = basepath(list[i]) }
                         end
        end

        def as_get(val, from)
          (global && @as[from][val]) || val
        end

        def unpack_get(*)
          nil
        end

        def task_build(keys)
          namespace name do
            ws = workspace
            keys.each do |key|
              next unless ws.task_include?(self, key)

              action = ws.series.name_get(key)
              unless @pass.include?(key.to_s) || ws.task_defined?(name, action) || ws.task_exclude?(action, self)
                ws.task_desc(@desc, action)
                task action do
                  __send__ key
                end
              end
              next if (items = children.select { |item| item.task_include?(key) }).empty?

              ws.task_desc(@desc, action, 'workspace')
              task task_join(action, 'workspace') => items.map! { |item| task_join(item.name, action) }
            end
          end
        end

        def task_pass?(key)
          @only ? !@only.include?(key) : @pass.include?(key)
        end

        def matchany?(val, list, empty: true)
          list.empty? ? empty : list.any? { |pat| val.match?(pat) }
        end

        def projectpath?(val)
          ret = Pathname.new(val).cleanpath
          ret.absolute? ? ret.to_s.start_with?(File.join(path, '')) : !ret.to_s.start_with?(File.join('..', ''))
        end

        def checkdir?(val)
          return true if val.directory? && !val.empty?

          log&.warn "directory \"#{val}\"".subhint(val.directory? ? 'empty' : 'missing')
          false
        end

        def semmajor?(cur, want)
          (cur[0] == '0' && want[0] == '0' ? cur[2] != want[2] : cur[0] != want[0]) && !want[5]
        end

        def printfirst?
          @@print_order == 0
        end

        def runnable?(val)
          case val
          when String, Enumerable, Proc, Method, Struct
            true
          else
            false
          end
        end

        def series?(val)
          val.is_a?(Array) && val.all? { |p| p.is_a?(Proc) || p.is_a?(Method) }
        end

        def from_base?(val)
          task_invoked?(val, "#{val}:sync", 'default')
        end

        def from_sync?(*val)
          if task_invoked?(key = task_join(*val))
            !workspace.task_defined?(key, 'sync')
          elsif workspace.series.sync?(task_join(key, 'sync'))
            true
          end
        end

        def invoked_sync?(action, val = nil)
          return true if val || from_sync?(ac = workspace.task_name(action))
          return val if group && !(val = from_sync?(ac, group)).nil?
          return val if (base = workspace.find_base(self)) && !(val = from_sync?(ac, base.ref)).nil?
          return false if workspace.series.chain?(key = task_join(name, action))
          return true if task_invoked?(key) && (!task_invoked?(ac) || !workspace.task_defined?(ac, 'sync'))

          ret = workspace.series.name_get(action)
          ret != action && invoked_sync?(ret)
        end

        def success?(run, *cond)
          case run
          when TrueClass
            true
          when FalseClass
            false
          else
            $?.success?
          end.tap do |ret|
            next unless cond.none? { |val| val == false }

            if block_given?
              yield ret
            elsif ret && stdout? && banner?
              print_success
            end
          end
        end

        def banner?
          ARG[:BANNER] && !env('BANNER', equals: '0')
        end

        def pwd?
          path == Pathname.pwd
        end

        def stdin?
          pipe == 0
        end

        def stdout?
          !!verbose && !stdin?
        end

        def verbose?
          verbosetype > 1
        end

        def silent?
          verbosetype == 0
        end

        def warning?
          workspace.warning
        end

        def has_value?(target, *args)
          return false unless target.is_a?(Enumerable)

          args = args.first if args.size == 1 && args.first.is_a?(Enumerable)
          args.any? { |obj,| target.include?(obj) }
        end

        def has_value!(target, *args, first: false)
          return unless target.is_a?(Enumerable)

          args = args.first if args.size == 1 && args.first.is_a?(Enumerable)
          found = false
          args.each do |val|
            if target.respond_to?(:delete?)
              found = true if target.delete?(val)
            elsif target.respond_to?(:delete)
              found = true if target.delete(val)
            elsif target.include?(val)
              found = true
            end
            break if found && first
          end
          target if found
        end

        def variables
          VAR_SET
        end

        def blocks
          BLK_SET
        end

        def borderstyle
          workspace.banner_get(*@ref, group: group)&.border || theme[:border]
        end

        def headerstyle
          opt_style theme[:header], /^(\S+)(\s+)$/
        end

        def scriptargs
          { target: script? ? @output[1] : @output[0], script: script?, ref: ref, group: group, global: global }
        end
      end

      Application.implement(Base, base: true)
      Application.attr_banner = Set.new(%i[name project path ref group parent])
    end

    module Git
      GIT_REPO = Support.hashobj
      GIT_PROTO = %r{\A(https?|ssh|git|file)://}i.freeze
      private_constant :GIT_REPO, :GIT_PROTO

      attr_reader :revfile

      def git(name, uri = nil, base: nil, repo: [], options: {}, cache: nil)
        data = {}
        check = ->(proj) { proj.is_a?(Project::Git) && !proj.exclude?(Project::Git.ref) && git_clone?(proj.path) }
        if uri.is_a?(Array)
          base = name
          uri.each do |val|
            if (proj = @project[val.to_s]) && check.call(proj)
              repo << proj
            end
          end
        elsif uri
          data[name.to_s] = uri
        else
          case name
          when Enumerable
            data = name.to_h
          when GIT_PROTO
            base = name.to_s
            each { |proj| repo << proj if !proj.parent && check.call(proj) }
          else
            warn log_warn(name, subject: 'git', hint: 'invalid') if warning
            return self
          end
        end
        if base
          base = base.match?(GIT_PROTO) ? "#{base.chomp('/')}/" : rootpath(base)
          repo.each do |target|
            if target.is_a?(Project::Git)
              data[target.localname] = target.project
            else
              data[target.to_s] = nil
            end
          end
        end
        data.each do |key, val|
          uri = if val.is_a?(Hash)
                  opts = val.fetch(:options, {})
                  val.fetch(:uri, '')
                else
                  opts = options
                  val.is_a?(String) ? val : key.to_s
                end
          unless uri.match?(GIT_PROTO) || Pathname.new(uri).absolute?
            if uri.start_with?('.')
              uri = rootpath uri
            elsif base
              uri = base + uri
            else
              next
            end
          end
          GIT_REPO[main][key = task_name(key)] = [uri.to_s, opts]
          @kind[key] << Project::Git
        end
        if cache == true
          revbuild
        elsif cache
          revbuild(file: cache)
        end
        self
      end

      def git_repo(name)
        (ret = GIT_REPO[main]) && ret[name]
      end

      def git_clone?(path, name = nil)
        return false if name && !git_repo(name)

        !path.exist? || path.empty?
      end

      def revbuild(file: nil)
        @revfile = @home.join(file || "#{@main}.revb")
        @revdoc = JSON.parse(@revfile.read) if @revfile.exist?
      rescue StandardError => e
        @revfile = nil
        warn log_warn(e, pass: true)
        self
      else
        @revdoc = {} unless @revdoc.is_a?(Hash)
        self
      end

      def rev_entry(*keys, val: nil, create: true)
        return unless @revdoc
        return @revdoc.dig(*keys) unless val

        data = @revdoc
        last = keys.pop
        keys.each do |key|
          if data[key].is_a?(Hash)
            data = data[key]
          elsif create
            data = data[key] = {}
          else
            return nil
          end
        end
        data[last] = val
      end

      def rev_timeutc(*keys)
        rev_entry(*keys, val: time_epoch)
      end

      def rev_timesince(*keys, clock: false)
        epoch = time_epoch - rev_entry(*keys).to_i
      rescue StandardError
        nil
      else
        time_format(epoch, clock: clock)
      end

      def rev_clear(name, sync: true)
        if Dir.exist?(name) && (proj = find(name))
          name = proj.name
        end
        rev_write(sync: sync) if rev_entry(name, 'revision', val: '', create: false)
      end

      def rev_write(name = nil, data = nil, sync: true, utc: nil)
        return unless @revfile

        sleep 0 while !sync && @revlock
        @revlock = true
        if name
          data&.each { |key, val| rev_entry(name, key, val: val) }
          rev_timeutc(name, utc) if utc
        end
        File.write(@revfile, JSON.pretty_generate(@revdoc))
      rescue StandardError => e
        log&.debug e
        warn log_warn(e, pass: true) if warning
      ensure
        @revlock = false
      end
    end

    Application.include Git

    module Project
      class Git < Base
        OPT_GIT = {
          common: %w[c=q bare glob-pathspecs icase-pathspecs literal-pathspecs no-optional-locks no-pager
                     no-replace-objects noglob-pathspecs paginate attr-source=b config-env=q exec-path=p
                     namespace=p].freeze,
          add: %w[A e|edit f|force ignore-errors ignore-missing i|interactive n|dry-run p|patch pathspec-file-nul
                  refresh renormalize sparse u|update v|verbose chmod=b pathspec-from-file=p].freeze,
          branch: %w[a|all create-reflog i|ignore-case omit-empty q|quiet r|remotes v|verbose=+ abbrev=i color=b
                     column=b contains=b format=q merged=b no-contains=b no-merged=b points-at=b u|set-upstream-to=b
                     sort=q t|track=b].freeze,
          checkout: %w[l d|detach f|force ignore-other-worktrees ignore-skip-worktree-bits m|merge p|patch
                       pathspec-file-nul q|quiet ours theirs conflict=b orphan=b pathspec-from-file=p t|track=b].freeze,
          diff: {
            base: %w[0 1|base 2|ours 3|theirs].freeze,
            show: %w[s exit-code histogram].freeze
          }.freeze,
          fetch: {
            base: %w[multiple porcelain progress P|prune-tags refetch stdin u|update-head-ok
                     recurse-submodules-default=b].freeze,
            pull: %w[4 6 n t a|append atomic dry-run f|force k|keep negotiate-only prefetch p|prune q|quiet set-upstream
                     unshallow update-shallow v|verbose deepen=i depth=i j|jobs=i negotiation-tip=q recurse-submodules=v
                     refmap=q o|server-option=q shallow-exclude=b shallow-since=v upload-pack=q].freeze
          }.freeze,
          git: {
            add: %w[N|intent-to-add refresh].freeze,
            blame: %w[b c l s t w C=im? L=q M=im? S=p color-by-age color-lines first-parent incremental line-porcelain
                      p|porcelain root score-debug f|show-name e|show-email n|show-number show-stats abbrev=i contents=p
                      date=q encoding=b ignore-rev=b ignore-revs-file=p reverse=q].freeze,
            clean: %w[d x X f|force n|dry-run i|interactive q|quiet e|exclude=q].freeze,
            grep: %w[e f=p h H I O=bm r all-match and G|basic-regexp break cached column c|count E|extended-regexp
                     l|files-with-matches L|files-without-match F|fixed-strings full-name W|function-context heading
                     i|ignore-case v|invert-match n|line-number name-only no-index not z|null o|only-matching or
                     P|perl-regexp q|quiet recurse-submodules p|show-function a|text untracked w|word-regexp
                     A|after-context=i B|before-context=i color=b C|context=i m|max-count=n max-depth=i
                     open-files-in-pager=b threads=n].freeze,
            mv: %w[k f|force n|dry-run v|verbose].freeze,
            revert: %w[e S=bm? n|no-commit reference cleanup=b gpg-sign=b? m|mainline=i s|signoff strategy=b
                       X|strategy-option=b].freeze,
            rm: %w[r cached f|force n|dry-run ignore-unmatch pathspec-file-nul q|quiet sparse
                   pathspec-from-file=p].freeze
          }.freeze,
          log: {
            base: %w[L=qm all all-match alternate-refs author-date-order basic-regexp bisect boundary cherry cherry-mark
                     cherry-pick clear-decorations date-order dense do-walk exclude-first-parent-only E|extended-regexp
                     first-parent F|fixed-strings follow full-diff full-history ignore-missing invert-grep left-only
                     log-size merge no-max-parents no-min-parents not P|perl-regexp reflog i|regexp-ignore-case
                     remove-empty reverse right-only simplify-by-decoration simplify-merges single-worktree show-pulls
                     source sparse stdin topo-order g|walk-reflogs after=q ancestry-path=b? author=q before=q
                     branches=q? committer=q decorate=b decorate-refs=q decorate-refs-exclude=q exclude=q
                     exclude-hidden=b glob=q grep=q grep-reflog=q n|max-count=i max-parents=i min-parents=i no-walk=b?
                     remotes=q? since=q since-as-filter=q skip=i tags=q? until=q].freeze,
            format: %w[t children combined-all-paths dd oneline left-right no-diff-merges parents relative-date
                       show-notes-by-default show-signature date=q diff-merges=b encoding=b expand-tabs=i format=q
                       notes=b pretty=q? show-linear-break=q?].freeze,
            diff: %w[p R u z B=bm? C=bm? l=im G=qm I=qm M=bm? O=qm S=qm binary check compact-summary cumulative
                     find-copies-harder full-index W|function-context w|ignore-all-space ignore-blank-lines
                     ignore-cr-at-eol ignore-space-at-eol b|ignore-space-change D|irreversible-delete graph
                     ita-invisible-in-index minimal name-only name-status no-color-moved-ws no-prefix no-renames numstat
                     patch-with-raw patch-with-stat patience pickaxe-all pickaxe-regex raw shortstat summary a|text
                     abbrev=i? anchored=q break-rewrites=b? color=b color-moved=b color-moved-ws=b color-words=q?
                     diff-algorithm=b diff-filter=e? X|dirstat=b? dirstat-by-file=b? dst-prefix=q find-copies=i?
                     find-object=b find-renames=b? ignore-matching-lines=q ignore-submodules=b? line-prefix=q output=p
                     output-indicator-context=q output-indicator-new=q output-indicator-old=q relative=p rotate-to=p
                     skip-to=p src-prefix=q stat=b? stat-count=i stat-width=i stat-name-width=i submodule=b?
                     word-diff=b? word-diff-regex=q ws-error-highlight=b].freeze,
            diff_context: %w[U=im inter-hunk-context=i unified=i].freeze
          }.freeze,
          ls_files: %w[f t v z debug deduplicate directory eol error-unmatch exclude-standard full-name i|ignored
                       k|killed no-empty-directory recurse-submodules sparse s|stage u|unmerged abbrev=i x|exclude=q
                       X|exclude-from=p exclude-per-directory=p format=q with-tree=q].freeze,
          ls_remote: %w[exit-code get-url q|quiet symref o|server-option=q sort=q upload-pack=q].freeze,
          merge: %w[e n S=bm? allow-unrelated-histories compact-summary ff-only m=q q|quiet v|verbose cleanup=b F|file=p
                    gpg-sign=b? into-name=b log=i s|strategy=b X|strategy-option=b].freeze,
          pull: %w[e n S=bm? allow-unrelated-histories compact-summary ff-only cleanup=b gpg-sign=b? log=i r|rebase=v?
                   s|strategy=b X|strategy-option=b].freeze,
          rebase: %w[n C=im S=bm? allow-empty-message apply committer-date-is-author-date edit-todo empty=b
                     f|force-rebase ignore-date ignore-whitespace i|interactive keep-base m|merge no-ff q|quiet quit
                     reset-author-date root show-current-patch signoff v|verbose empty=b x|exec=q gpg-sign=b? onto=b
                     r|rebase-merges=b s|strategy=b X|strategy-option=b whitespace=b].freeze,
          reset: %w[N pathspec-file-nul q|quiet pathspec-from-file=p].freeze,
          restore: %w[ignore-skip-worktree-bits ignore-unmerged m|merge ours p|patch pathspec-file-nul q|quiet S|staged
                      theirs W|worktree conflict=b pathspec-from-file=p s|source=b].freeze,
          rev_parse: %w[absolute-git-dir all git-common-dir git-dir is-bare-repository is-inside-git-dir
                        is-inside-work-tree is-shallow-repository local-env-vars no-revs not q|quiet revs-only
                        shared-index-path show-cdup show-prefix show-ref-format show-superproject-working-tree
                        show-toplevel sq sq-quote symbolic symbolic-full-name verify abbrev-ref=b? after=q before=q
                        branches=q? default=q disambiguate=b exclude=q exclude-hidden=b git-path=p glob=q
                        output-object-format=b path-format=b? prefix=q remotes=q? resolve-git-dir=p short=i?
                        show-object-format=b? since=q tags=q? until=q].freeze,
          show: %w[t combined-all-paths no-diff-merges remerge-diff show-notes-by-default show-signature diff-merges=b
                   encoding=b expand-tabs=i notes=q show-notes=q?].freeze,
          stash: {
            common: %w[q|quiet].freeze,
            push: %w[a|all u|include-untracked k|keep-index no-keep-index no-include-untracked pathspec-file-nul p|patch
                     S|staged m|message=q pathspec-from-file=p].freeze,
            pop: %w[index].freeze,
            apply: %w[index].freeze
          }.freeze,
          status: %w[z u=bm? b|branch long s|short show-stash v|verbose=+ column=b find-renames=i? ignore-submodules=b?
                     ignored=b? porcelain=b? untracked-files=b?].freeze,
          submodule: {
            status: %w[cached recursive].freeze,
            update: %w[checkout f|force init merge N|no-fetch no-recommend-shallow no-single-branch recommend-shallow
                       rebase recursive remote single-branch depth=i filter=q jobs=i reference=b ref-format=q].freeze,
            branch: %w[b|branch d|default].freeze,
            sync: %w[recursive].freeze
          }.freeze,
          switch: %w[d|detach discard-changes f|force ignore-other-worktrees m|merge q|quiet conflict=b c|create=q
                     C|force-create=q orphan=q t|track=b].freeze,
          tag: %w[n=im cleanup=b create-reflog i|ignore-case omit-empty color=b? column=b contains=b? format=q merged=b?
                  no-contains=b? no-merged=b? points-at=q sort=q trailer=q].freeze,
          no: {
            add: %w[all ignore-removal].freeze,
            blame: %w[progress].freeze,
            branch: %w[color color-moved column track].freeze,
            checkout: %w[overwrite-ignore guess overlay progress recurse-submodules track].freeze,
            fetch: {
              base: %w[auto-gc auto-maintenance write-commit-graph write-fetch-head].freeze,
              pull: %w[all ipv4 ipv6 recurse-submodules show-forced-updates tags].freeze
            },
            grep: %w[color exclude-standard recursive textconv].freeze,
            log: {
              base: %w[decorate mailmap merges use-mailmap].freeze,
              diff: %w[color color-moved ext-diff indent-heuristic patch relative rename-empty textconv].freeze,
              show: %w[abbrev-commit expand-tabs notes].freeze
            }.freeze,
            merge: %w[autostash edit ff gpg-sign log overwrite-ignore progress rerere-autoupdate signoff squash stat
                      verify verify-signatures].freeze,
            pull: %w[autostash commit edit gpg-sign ff log rebase signoff squash stat verify verify-signatures].freeze,
            rebase: %w[autosquash autostash fork-point gpg-sign keep-empty reapply-cherry-picks rebase-merges
                       rerere-autoupdate reschedule-failed-exec stat update-refs verify].freeze,
            reset: %w[refresh].freeze,
            restore: %w[overlay progress recurse-submodules].freeze,
            rev_parse: %w[flags].freeze,
            revert: %w[edit gpg-sign rerere-autoupdate].freeze,
            show: %w[standard-notes].freeze,
            status: %w[ahead-behind column renames].freeze,
            switch: %w[guess progress recurse-submodules track].freeze,
            tag: %w[column].freeze
          }.freeze
        }.freeze
        VAL_GIT = {
          merge: {
            send: %w[continue abort quit].freeze
          }.freeze,
          rebase: {
            send: %w[continue skip abort quit].freeze,
            value: %w[true false merges interactive].freeze
          }.freeze,
          reset: %w[soft mixed hard merge keep recurse-submodules no-recurse-submodules].freeze,
          revbuild: %w[untracked-files ignore-submodules ignored].freeze
        }.freeze
        private_constant :OPT_GIT, :VAL_GIT

        class << self
          include Rake::DSL

          def tasks
            %i[pull rebase autostash fetch clone stash status branch revbuild].freeze
          end

          def config?(val)
            return false unless (val = as_path(val))

            val.join('.git').directory?
          end
        end

        subtasks({
          'branch' => %i[create track delete move copy list current].freeze,
          'checkout' => %i[commit branch track detach path].freeze,
          'commit' => %i[add all amend amend-orig fixup].freeze,
          'diff' => %i[head branch files view between contain].freeze,
          'fetch' => %i[origin remote all].freeze,
          'files' => %i[cached modified deleted others].freeze,
          'git' => %i[add blame clean grep mv revert rm status].freeze,
          'log' => %i[view grep between contain].freeze,
          'merge' => %i[commit no-commit send].freeze,
          'pull' => %i[origin remote all].freeze,
          'rebase' => %i[branch onto send].freeze,
          'refs' => %i[heads tags remote].freeze,
          'reset' => %i[commit index patch mode undo].freeze,
          'restore' => %i[source staged worktree].freeze,
          'rev' => %i[commit build output].freeze,
          'show' => %i[format oneline textconv].freeze,
          'stash' => %i[push pop apply branch drop clear list all staged worktree].freeze,
          'submodule' => %i[status update branch url sync].freeze,
          'switch' => %i[branch create detach].freeze,
          'tag' => %i[add sign delete list].freeze
        })

        def initialize(*, **)
          super
          @submodule = exist?('.gitmodules')
          initialize_ref Git.ref if gitpath.exist?
        end

        def ref
          Git.ref
        end

        def populate(*, **)
          super
          return unless ref?(Git.ref) || @only

          namespace name do
            Git.subtasks do |action, flags|
              next if task_pass?(action)

              namespace action do
                flags.each do |flag|
                  case action
                  when 'pull', 'fetch'
                    if flag == :remote
                      format_desc action, flag, 'remote?,opts*'
                      task flag, [:remote] do |_, args|
                        args = if (remote = args.remote)
                                 args.extras
                               else
                                 remote = choice_remote
                                 args.to_a
                               end
                        __send__(action, flag, args, remote: remote)
                      end
                    else
                      format_desc(action, flag, 'opts*', after: ('pattern*' if flag == :all && action == 'pull'))
                      task flag do |_, args|
                        __send__ action, flag, args.to_a
                      end
                    end
                  when 'submodule'
                    break unless @submodule

                    case flag
                    when :branch
                      format_desc action, flag, 'path,name?'
                      task flag, [:path, :name] do |_, args|
                        path = param_guard(action, flag, args: args, key: :path)
                        branch = args.name
                        submodule(flag, [branch ? 'b' : 'd'], branch: branch, path: path)
                      end
                    when :url
                      format_desc action, flag, 'path,url,opts*'
                      task flag, [:path, :url] do |_, args|
                        path = param_guard(action, flag, args: args, key: :path)
                        url = param_guard(action, flag, args: args, key: :url)
                        submodule(flag, args.extras, path: path, url: url)
                      end
                    else
                      format_desc action, flag, 'opts*,path*'
                      task flag do |_, args|
                        submodule flag, args.to_a
                      end
                    end
                  when 'commit'
                    case flag
                    when :all
                      format_desc action, flag, 'message?'
                      task flag, [:message] do |_, args|
                        commit(flag, message: args.message)
                      end
                    else
                      format_desc(action, flag, 'pathspec+', before: ('opts*' if flag == :add))
                      task flag do |_, args|
                        if flag == :fixup
                          ref, squash, pick = choice_commit(reflog: false, accept: [accept_b('Auto squash?')],
                                                            values: 'Pick [amend|reword]')
                          pick &&= case pick.downcase
                                   when 'a', 'amend'
                                     'amend'
                                   when 'r', 'reword'
                                     'reword'
                                   end
                          if squash
                            found = false
                            git_spawn(git_output('log --format=%h'), stdout: false).each do |val|
                              if found
                                squash = val.chomp
                                break
                              end
                              found = val.chomp == ref
                            end
                          end
                        end
                        opts = []
                        refs = []
                        unless pick == 'reword'
                          if flag == :add
                            opts = param_guard(action, flag, args: args.to_a)
                          elsif (refs = args.to_a).empty?
                            refs = readline('Enter file patterns', force: true).shellsplit
                          end
                        end
                        commit(flag, opts, refs: refs, ref: ref, squash: squash, pick: pick)
                      end
                    end
                  when 'tag'
                    case flag
                    when :list
                      format_desc action, flag, 'opts*,pattern*'
                      task flag do |_, args|
                        tag flag, args.to_a
                      end
                    when :delete
                      format_desc action, flag, 'name+'
                      task flag do |_, args|
                        refs = args.to_a
                        if refs.empty?
                          refs = choice_refs('Choose a tag', 'tags', multiple: true, series: true, accept: 'Delete?')
                          remote = choice_remote
                        end
                        tag(flag, refs: refs, remote: remote)
                      end
                    when :add, :sign
                      format_desc action, flag, 'name,message?,commit?,remote?'
                      task flag, [:name, :message, :commit, :remote] do |_, args|
                        remote = if (name = args.name)
                                   message = args.message
                                   commit = commithead args.commit
                                   args.remote
                                 else
                                   commit, name, message = choice_commit(reflog: false, series: true,
                                                                         values: [
                                                                           ['Enter tag name', true],
                                                                           'Enter message'
                                                                         ])
                                   choice_remote
                                 end
                        tag(flag, refs: [name], message: message, commit: commit, remote: remote).tap do |ret|
                          success?(ret, !remote)
                        end
                      end
                    end
                  when 'stash'
                    format_desc(action, flag, 'opts*', after: case flag
                                                              when :push then 'pathspec*,:'
                                                              when :branch then 'name,stash/:'
                                                              when :clear, :list, :all then nil
                                                              else 'stash?|:'
                                                              end)
                    task flag do |_, args|
                      stash flag, args.to_a
                    end
                  when 'log', 'diff'
                    case flag
                    when :view, :between, :contain
                      if action == 'log' && flag == :view
                        format_desc action, flag, '(^)commit*|:,opts*,pathspec*'
                        task flag do |_, args|
                          args = args.to_a
                          if args.first == ':'
                            args.shift
                            index = choice_commit(multiple: true)
                          else
                            index = []
                            args.each do |val|
                              if matchhead(val)
                                index << commithead(val)
                              elsif (sha = commithash(val))
                                index << sha
                              elsif val.start_with?('^')
                                index << shell_quote(val)
                              else
                                break
                              end
                            end
                            args = args.drop(index.size)
                          end
                          log!(flag, args, index: index)
                        end
                      else
                        format_desc action, flag, 'commit1,commit2,opts*,pathspec*'
                        task flag, [:commit1, :commit2] do |_, args|
                          commit1 = commithead args.commit1
                          range = if commit1
                                    commit2 = commithead param_guard(action, flag, args: args, key: :commit2)
                                    args = args.extras
                                    [commit1, commit2]
                                  else
                                    range, opts, refs = choice_commit(multiple: flag == :view ? true : 2,
                                                                      values: %w[Options Pathspec])
                                    args = OptionPartition.strip(opts)
                                    args.concat(refs.shellsplit) if refs
                                    range.reverse
                                  end
                          __send__(action == 'log' ? :log! : :diff, flag, args, range: range)
                        end
                      end
                    when :head
                      format_desc action, flag, 'commit*|:,opts*,pathspec*'
                      task flag do |_, args|
                        args = args.to_a
                        if args.first == ':'
                          args.shift
                          index = choice_commit(multiple: true)
                        else
                          index = []
                          args.each do |val|
                            break unless (sha = commithead(val) || commithash(val))

                            index << sha
                          end
                          args = args.drop(index.size)
                        end
                        diff(flag, args, index: index)
                      end
                    when :branch
                      format_desc action, flag, 'name,opts*,pathspec*'
                      task flag, [:name] do |_, args|
                        branch = param_guard(action, flag, args: args, key: :name)
                        diff(flag, args.extras, branch: branch)
                      end
                    when :files
                      format_desc action, flag, 'path1,path2,patch?'
                      task flag, [:path1, :path2, :patch] do |_, args|
                        path1 = param_guard(action, flag, args: args, key: :path1)
                        path2 = param_guard(action, flag, args: args, key: :path2)
                        diff(flag, refs: [path1, path2, args.patch])
                      end
                    when :grep
                      format_desc action, flag, 'pattern+,a/ll-match?,in/vert-grep?,i/E/F/P?,max-count?=i,f/ormat?=s'
                      task flag do |_, args|
                        grep = args.to_a
                        opts = ['oneline']
                        while (last = grep.pop)
                          case last
                          when '--'
                            grep << '--' if grep.empty?
                            break
                          when /^a(ll-match)?$/
                            opts << 'all-match'
                          when /^in(vert-grep)?$/
                            opts << 'invert-grep'
                          when 'i', 'E', 'F', 'P'
                            opts << last
                          else
                            if last =~ /^(f(-ormat)?)=(.+)$/
                              opts.shift
                              opts << "format=#{$1}"
                            elsif last =~ /^(max(-count)?)=(\d+)$/
                              opts << "max-count=#{$1}"
                            else
                              grep << last
                              break
                            end
                          end
                        end
                        param_guard(action, flag, args: grep)
                        log!(flag, opts, grep: grep)
                      end
                    end
                  when 'checkout'
                    case flag
                    when :branch
                      format_desc action, flag, 'name,create?=[bB],commit?,d/etach?'
                      task flag, [:name, :create, :commit, :detach] do |_, args|
                        if (branch = args.name)
                          branch = param_guard(action, flag, args: args, key: :name)
                          create = args.create
                          detach = if args.commit == 'd'
                                     commit = nil
                                     'd'
                                   elsif create == 'd'
                                     create = nil
                                     commit = nil
                                     'd'
                                   elsif create && create.size > 1
                                     commit = commithead create
                                     create = nil
                                     args.commit
                                   else
                                     commit = commithead args.commit
                                     args.detach
                                   end
                          param_guard(action, flag, args: { create: create }, key: :create, pat: /\A[Bb]\z/) if create
                        else
                          branch = choice_refs 'Choose a branch to switch'
                        end
                        checkout(flag, branch: branch, create: create, commit: commit, detach: detach)
                      end
                    when :track
                      format_desc action, flag, 'origin,(^)name?'
                      task flag, [:origin, :name] do |_, args|
                        if (origin = args.origin)
                          branch = args.name
                        else
                          origin, branch = choice_refs('Choose a remote', 'remotes', values: 'Enter branch name')
                        end
                        checkout(flag, branch: branch, origin: origin)
                      end
                    when :commit
                      format_desc action, flag, 'ref,opts*'
                      task flag, [:commit] do |_, args|
                        commit = commithead args.commit
                        args = if commit
                                 args.extras
                               else
                                 commit, opts = choice_commit(values: 'Options')
                                 OptionPartition.strip(opts)
                               end
                        checkout(flag, args, commit: commit)
                      end
                    when :detach
                      format_desc action, flag, 'ref?'
                      task flag, [:commit] do |_, args|
                        commit = commithead args.commit
                        unless commit
                          commit, merge = choice_commit(values: 'Merge? [y/N]')
                          merge = merge&.upcase == 'Y'
                        end
                        checkout(flag, commit: commit, merge: merge)
                      end
                    when :path
                      format_desc action, flag, 'opts*,pathspec*'
                      task flag do |_, args|
                        checkout flag, args.to_a
                      end
                    end
                  when 'branch'
                    case flag
                    when :create
                      format_desc action, flag, 'name,ref/:'
                      task flag, [:name, :ref] do |_, args|
                        target = param_guard(action, flag, args: args, key: :name)
                        ref = commithead args.ref
                        if ref == ':'
                          ref, remote = choice_refs('Choose a remote', 'remotes', accept: [accept_b('Push?')])
                        end
                        branch(flag, target: target, ref: ref, remote: remote)
                      end
                    when :track
                      format_desc action, flag, '(^~)upstream?,name?'
                      task flag, [:upstream, :name] do |_, args|
                        if (ref = args.upstream)
                          target = args.name
                          remote = true if ref.delete_prefix!('~')
                        else
                          ref, remote, target = choice_refs('Choose a remote', 'remotes', accept: [accept_b('Push?')],
                                                                                          values: 'Enter branch name')
                        end
                        branch(flag, target: target, ref: ref, remote: remote)
                      end
                    when :delete
                      format_desc action, flag, '[^~]name*,:?'
                      task flag do |_, args|
                        refs = args.to_a
                        if refs.empty? || (r = refs.last == ':')
                          accept = ['Delete?']
                          accept << accept_b('Force?') unless r
                          remote = choice_refs('Choose a branch', r ? 'remotes' : 'heads', multiple: true,
                                                                                           accept: accept)
                          if r
                            refs.pop
                          else
                            refs = remote.first
                            refs.map! { |val| "^#{val}" } if remote[1]
                            remote = nil
                          end
                        end
                        branch(flag, refs: refs, remote: remote)
                      end
                    when :list
                      format_desc action, flag, 'opts*,pattern*'
                      task flag do |_, args|
                        branch flag, args.to_a
                      end
                    when :current
                      format_desc action, flag
                      task flag do
                        branch flag
                      end
                    else
                      format_desc action, flag, 'branch,oldbranch?'
                      task flag, [:branch, :oldbranch] do |_, args|
                        if (branch = args.branch)
                          oldbranch = args.oldbranch
                        else
                          oldbranch, branch = choice_refs("Choose a branch to #{flag}",
                                                          values: [['Enter new branch name', true]])
                        end
                        branch(flag, refs: [oldbranch, branch])
                      end
                    end
                  when 'switch'
                    case flag
                    when :create
                      format_desc action, flag, '(^)name,ref/:'
                      task flag, [:name, :commit] do |_, args|
                        branch = param_guard(action, flag, args: args, key: :name)
                        commit = commithead args.commit
                        commit, track = choice_commit(force: false, values: 'Track? [Y/n]') if commit == ':'
                        switch(flag, branch: branch, commit: commit, track: track)
                      end
                    when :detach
                      format_desc action, flag, 'ref?'
                      task flag, [:commit] do |_, args|
                        commit = commithead(args.commit) || choice_commit(force: false)
                        switch(flag, commit: commit)
                      end
                    when :branch
                      format_desc action, flag, 'name/:,opts*'
                      task flag, [:name] do |_, args|
                        args = if (branch = args.name)
                                 branch = nil if branch == ':'
                                 args.extras
                               else
                                 []
                               end
                        switch(flag, args, branch: branch || choice_refs('Choose a branch'))
                      end
                    end
                  when 'reset'
                    case flag
                    when :commit
                      format_desc action, flag, 'ref/:,opts*'
                      task flag, [:commit] do |_, args|
                        commit = commithead args.commit
                        args = if commit && commit != ':'
                                 args.extras
                               else
                                 commit, mode = choice_commit(values: ['Mode [mixed|soft|hard|N]'])
                                 args.extras.concat(case mode&.downcase
                                                    when 'h', 'hard' then ['hard']
                                                    when 's', 'soft' then ['soft']
                                                    when 'n', 'N' then %w[mixed N]
                                                    else ['mixed']
                                                    end)
                               end
                        success?(reset(flag, args, commit: commit))
                      end
                    when :index, :undo
                      format_desc(action, flag, ('opts*,pathspec*' if flag == :index))
                      task flag do |_, args|
                        reset(flag, flag == :index ? args.to_a : [])
                      end
                    when :mode
                      format_desc action, flag, 'mode,ref/:'
                      task flag, [:mode, :ref] do |_, args|
                        mode = param_guard(action, flag, args: args, key: :mode)
                        ref = commithead args.ref
                        ref = choice_commit(reflog: false) if ref == ':'
                        reset(flag, mode: mode, ref: ref)
                      end
                    when :patch
                      format_desc action, flag, 'ref/:,pathspec*'
                      task flag, [:ref] do |_, args|
                        ref = commithead args.ref
                        ref = choice_commit(reflog: false) unless ref && ref != ':'
                        reset(flag, refs: args.extras, ref: ref)
                      end
                    end
                  when 'show'
                    case flag
                    when :oneline
                      format_desc action, flag, 'opts*,object*'
                      task flag do |_, args|
                        show flag, args.to_a
                      end
                    when :format
                      format_desc action, flag, 'format?,opts*,object*'
                      task flag, [:format] do |_, args|
                        show(flag, args.extras, format: args.format)
                      end
                    when :textconv
                      format_desc action, flag, 'files+'
                      task flag do |_, args|
                        files = param_guard(action, flag, args: args.to_a)
                        show(flag, files: files)
                      end
                    end
                  when 'rebase', 'merge'
                    case flag
                    when :branch
                      format_desc action, flag, 'upstream,branch?,opts*'
                      task flag, [:upstream] do |_, args|
                        args = if (upstream = args.upstream)
                                 args.extras
                               else
                                 upstream, opts = choice_refs('Choose upstream branch', values: 'Options')
                                 OptionPartition.strip(opts)
                               end
                        rebase(flag, args, upstream: upstream)
                      end
                    when :onto
                      format_desc action, flag, 'ref,upstream,branch?'
                      task flag, [:commit, :upstream, :branch] do |_, args|
                        commit = commithead args.commit
                        args = if commit
                                 upstream = param_guard(action, flag, args: args, key: :upstream)
                                 branch = args.branch
                                 []
                               else
                                 commit = choice_refs 'Choose "onto" branch'
                                 target, opts = choice_commit(reflog: false, multiple: 2, values: 'Options')
                                 branch, upstream = target
                                 OptionPartition.strip(opts)
                               end
                        rebase(flag, args, commit: commit, upstream: upstream, branch: branch)
                      end
                    when :commit, :'no-commit'
                      format_desc action, flag, 'refs+,opts*'
                      task flag do |_, args|
                        args = args.to_a
                        if args.empty?
                          accept = "Merge with #{`#{git_output('branch --show-current')}`.chomp}?"
                          branch, opts = choice_refs('Choose a branch', values: 'Options', accept: accept)
                          args = OptionPartition.strip(opts)
                        end
                        merge(flag, args, branch: branch)
                      end
                    when :send
                      format_desc(action, flag, VAL_GIT[action.to_sym][:send], arg: nil)
                      task flag, [:command] do |_, args|
                        command = param_guard(action, flag, args: args, key: :command,
                                                            values: VAL_GIT[action.to_sym][:send])
                        __send__(action, flag, command: command)
                      end
                    end
                  when 'rev'
                    case flag
                    when :commit
                      format_desc action, flag, 'ref?,size?'
                      task flag, [:ref, :size] do |_, args|
                        ref = commithead args.ref
                        size = args.size
                        if !size && ref.to_i.between?(1, 40)
                          size = ref
                          ref = nil
                        end
                        rev_parse(flag, ref: ref, size: size)
                      end
                    when :build
                      next unless build?

                      format_desc action, flag, 'opts*'
                      task flag do |_, args|
                        revbuild flag, args.to_a
                      end
                    when :output
                      format_desc action, flag, 'opts*,args*'
                      task flag do |_, args|
                        rev_parse flag, args.to_a
                      end
                    end
                  when 'refs', 'files'
                    if flag == :remote
                      format_desc action, flag, 'remote?,opts*,pattern*'
                      task flag, [:remote] do |_, args|
                        ls_remote(flag, args.extras, remote: args.remote)
                      end
                    else
                      format_desc(action, flag, 'opts*,pattern*', after: ('pathspec*' if action == 'files'))
                      task flag do |_, args|
                        __send__(action == 'refs' ? :ls_remote : :ls_files, flag, args.to_a)
                      end
                    end
                  when 'restore'
                    case flag
                    when :source
                      format_desc action, flag, 'ref,opts*,pathspec*'
                      task flag, [:commit] do |_, args|
                        commit = commithead args.commit
                        args = if commit
                                 args.extras
                               else
                                 commit, opts, files = choice_commit(values: ['Options', ['Pathspec', true]])
                                 files = files&.shellsplit
                                 OptionPartition.strip(opts)
                               end
                        restore(flag, args, commit: commit, files: files)
                      end
                    when :staged, :worktree
                      format_desc action, flag, 'opts*,pathspec*|:'
                      task flag do |_, args|
                        args = args.to_a
                        if args.empty? || args.last == ':'
                          files = []
                          status_data.each { |row| files << row[0] if row[flag == :staged ? 2 : 1].match?(/[AMDRTC]/) }
                          unless files.empty?
                            files = choice_index('Select a file', files, multiple: true, force: false,
                                                                         accept: 'Restore?')
                          end
                          args.pop
                          args, glob = args.partition { |val| val.match?(/^(?:[a-z-]+=|[^*]+$)/) }
                          files.concat(glob)
                          next if args.empty? && files.empty?
                        end
                        restore(flag, args, files: files)
                      end
                    end
                  when 'git'
                    before = case flag
                             when :blame
                               'file'
                             when :mv
                               'source+,destination'
                             when :revert
                               'commit+'
                             end
                    after = case flag
                            when :add
                              'pathspec*,pattern*'
                            when :grep
                              'tree*,pathspec*'
                            when :clean, :rm, :status
                              'pathspec*'
                            end
                    format_desc(action, flag, 'opts*', before: before, after: after)
                    task flag do |_, args|
                      __send__(flag == :status ? :status : :git, flag, args.to_a)
                    end
                  end
                end
              end
            end
          end
        end

        def generate(keys = [], **)
          keys << :clone if clone?
          super
        end

        def depend(*, sync: invoked_sync?('depend'), **)
          workspace.rev_clear(name, sync: sync)
          super
        end

        def clean(*, sync: invoked_sync?('clean'), **)
          workspace.rev_clear(name, sync: sync)
          super
        end

        def pull(flag = nil, opts = [], sync: invoked_sync?('pull', flag), remote: nil, hint: nil)
          cmd, opts = git_session('pull', opts: opts)
          cmd << '--autostash' if option('autostash')
          if flag == :rebase
            cmd << '--rebase'
          else
            option('rebase', ignore: false) do |val|
              cmd << case val
                     when '0', 'false'
                       '--no-rebase'
                     else
                       VAL_GIT[:rebase][:value].include?(val) ? basic_option('rebase', val) : '--rebase'
                     end
            end
            case flag
            when :all
              unless git_spawn('status -s -z --untracked-files=all').empty?
                if confirm('Stash local changes?', 'Y')
                  git_spawn 'stash push --keep-index --quiet'
                elsif !(force = confirm('Force checkout?', 'N'))
                  return
                end
                printsucc
              end
              op = OptionPartition.new(opts, OPT_GIT[:pull], cmd, project: self, no: OPT_GIT[:no][:pull])
              reg = if op.empty?
                      []
                    else
                      opts = op.uniq(opts)
                      matchmap op
                    end
              session_done op.target
              heads = []
              cur = nil
              foreachref('heads', format: '%(if)%(HEAD)%(then)* %(end)%(refname:short)').each do |line|
                line.chomp!
                cur ||= line.delete_prefix!('* ')
                heads << line if matchany?(line, reg)
              end
              raise_error 'head not found', hint: 'for-each-ref' unless cur
              opts << 'ff-only' if opts.empty? && !option('ff-only', equals: '0')
              (heads.dup << cur).each_with_index do |branch, i|
                next unless (i < heads.size && cur != branch) || i == heads.size

                git_spawn 'switch --quiet', force && '--force', shell_quote(branch)
                pull(nil, opts, sync: false, hint: branch) if heads.include?(branch)
              end
              return
            when :autostash
              cmd << '--autostash'
            end
          end
          append_pull(opts, OPT_GIT[:pull] + OPT_GIT[:fetch][:pull],
                      flag: flag, from: :pull, remote: remote, no: OPT_GIT[:no][:pull] + OPT_GIT[:no][:fetch][:pull])
          source(sync: sync, sub: if stdout?
                                    [
                                      opt_style(color(:red), /^(.+)(\|\s+\d+\s+)([^-]*)(-+)(.*)$/, 4),
                                      opt_style(color(:green), /^(.+)(\|\s+\d+\s+)(\++)(.*)$/, 3)
                                    ]
                                  end, hint: hint, **threadargs)
        end

        def rebase(flag = nil, opts = [], sync: invoked_sync?('rebase', flag), commit: nil, upstream: nil, branch: nil,
                   command: nil)
          return pull(:rebase, sync: sync) unless flag

          cmd, opts = git_session('rebase', opts: opts)
          case flag
          when :branch
            return unless upstream

            op = OptionPartition.new(opts, OPT_GIT[:rebase], cmd, project: self, no: OPT_GIT[:no][:rebase])
            op << upstream
            append_head op.shift&.delete_prefix(':')
            op.clear(pass: false)
          when :onto
            return unless upstream

            cmd << '--interactive' if option('interactive', 'i')
            cmd << shell_option('onto', commit) if commit
            cmd << upstream
            append_head branch
          else
            unless gitpath('REBASE_HEAD').exist?
              puts log_message('no rebase in progress', subject: name, hint: command) if stdout?
              exit 1
            end
            return unless VAL_GIT[:rebase][:send].include?(command)

            cmd << "--#{command}"
          end
          source(sync: sync)
        end

        def autostash(*, sync: invoked_sync?('autostash'), **)
          pull(:autostash, sync: sync)
        end

        def fetch(flag = nil, opts = [], sync: invoked_sync?('fetch', flag), remote: nil)
          opts = git_session('fetch', opts: opts).last
          opts << 'all' if flag == :all || option('all')
          append_pull(opts, collect_hash(OPT_GIT[:fetch]), flag: flag, from: :fetch, remote: remote,
                                                           no: collect_hash(OPT_GIT[:no][:fetch]))
          source(sync: sync, **threadargs)
        end

        def clone(*, sync: invoked_sync?('clone'), **)
          return unless clone? && (data = workspace.git_repo(name))

          cmd = git_session('clone', worktree: false)
          opts = data[1].dup
          option('depth', ignore: false) do |val|
            if (n = val.to_i) > 0
              opts[:depth] = n
            else
              opts.delete(:depth)
            end
          end
          option('origin', ignore: false) { |val| opts[:origin] = val }
          if (branch = option('branch', strict: true))
            opts[:branch] = branch
            opts.delete(:revision)
          else
            option('revision', strict: true) do |val|
              opts[:revision] = val
              opts.delete(:branch)
              opts.delete(:mirror)
            end
          end
          option('local', strict: true) { |val| opts[:local] = val != '0' }
          option('bare') { |val| opts[:bare] = val }
          option('single-branch', ignore: false) do |val|
            opts[:'single-branch'] = val != '0' && val != 'false'
            opts.delete(:'no-single-branch')
          end
          option('no-checkout') do
            opts[:'no-checkout'] = true
            opts.delete(:n)
          end
          option('no-tags') { opts[:'no-tags'] = true }
          opts.delete(:'recurse-submodules') || opts.delete(:'no-recurse-submodules') if append_submodules(from: :clone)
          append_hash opts
          cmd << '--quiet' if option('quiet') || !verbose
          append_value(data[0], path, delim: true)
          source(sync: sync, banner: sync && !quiet?, multiple: !sync || quiet?)
        end

        def stash(flag = nil, opts = [], sync: invoked_sync?('stash', flag))
          if flag
            case flag
            when :all
              opts << 'include-untracked'
              flag = :push
            when :staged
              opts << 'staged'
              flag = :push
            when :worktree
              opts << 'keep-index'
              flag = :push
            end
            unless (file = gitpath('logs/refs/stash')).exist? || flag == :push
              puts log_message('no stashes were found', subject: name, hint: flag) if stdout?
              exit 1
            end
            cmd, opts = git_session('stash', flag, opts: opts)
            list = OPT_GIT[:stash][:common] + OPT_GIT[:stash].fetch(flag, [])
            if flag == :list
              list.concat(collect_hash(OPT_GIT[:log]))
              no = collect_hash OPT_GIT[:no][:log]
            end
            op = OptionPartition.new(opts, list, cmd, project: self, no: no, first: (matchpathspec if flag == :push))
            case flag
            when :push
              op.append?('message', readline('Enter message', force: true), force: true) if op.remove(':')
              append_pathspec op.extras
            when :pop, :apply, :drop, :branch
              if op.remove(':')
                if flag == :branch
                  if op.empty?
                    values = [['Branch name', true]]
                  else
                    op.add_first(prefix: ':')
                  end
                end
                out = choice_index('Choose a stash', git_spawn('stash list', stdout: false),
                                   values: values, column: /^[^@]+@\{(\d+)\}/)
                if values
                  op.merge(out.reverse)
                else
                  op << out
                end
              elsif !op.empty?
                op.add_first(prefix: ':')
              elsif flag == :branch
                raise_error ArgumentError, 'no branch name'
              end
              op.clear
            when :clear
              n = sub_style file.read.lines.size, theme[:inline]
              s = sub_style name, theme[:active]
              source(stdout: true) if confirm("Remove #{n} stash entries from #{s}?", 'N')
              return
            when :list
              op.clear
              out, banner, from = source(io: true)
              print_item banner
              list_result(write_lines(out), 'objects', from: from)
              return
            end
          else
            git_session('stash', 'push', opts: opts)
            append_option(OptionPartition.select(OPT_GIT[:stash][:push], no: false), no: true, ignore: false)
            append_message
          end
          source(sync: sync, banner: !quiet?, **threadargs)
        end

        def status(flag = nil, opts = [])
          cmd, opts = git_session('status', opts: opts)
          if flag
            op = OptionPartition.new(opts, OPT_GIT[:status], cmd, project: self, no: OPT_GIT[:no][:status])
            append_pathspec op.extras
          else
            cmd << (option('long') ? '--long' : '--short')
            cmd << '--branch' if option('branch')
            option('ignore-submodules', ignore: false) do |val|
              cmd << basic_option('ignore-submodules', case val
                                                       when '0', 'none'
                                                         'none'
                                                       when '1', 'untracked'
                                                         'untracked'
                                                       when '2', 'dirty'
                                                         'dirty'
                                                       else
                                                         'all'
                                                       end)
            end
            append_pathspec
          end
          if stdout?
            r = color(:red)
            g = color(:green)
            sub = if session_arg?('short')
                    [
                      opt_style(r, /^(.)([A-Z?!])(.+)$/, 2),
                      opt_style(g, /^([A-Z?!])(.+)$/),
                      opt_style(r, /^(\?\?)(.+)$/),
                      opt_style([nil, g, nil, r], /^(## )((?~\.{3}))(\.{3})(.+)$/, -1)
                    ]
                  else
                    opt_style(r, /^(\t+)([a-z]+: +.+)$/, 2)
                  end
          end
          out, banner, from = source(io: true)
          ret = write_lines(out, banner: banner, sub: sub)
          list_result(ret, 'files', action: 'modified', from: from)
        end

        def revbuild(flag = nil, opts = [], sync: nil, **kwargs)
          kw = lambda do
            {
              include: relativepath(*Array(kwargs[:include]), all: true),
              exclude: relativepath(*Array(kwargs[:exclude]), all: true)
            }
          end
          unless workspace.closed
            if @revbuild
              kw.call.each { |key, val| @revbuild[key] += val }
            else
              @revbuild = kw.call
            end
            return
          end
          sha = git_spawn('rev-parse --verify HEAD').chomp
          return if sha.empty?

          sync = invoked_sync?('revbuild', flag) if sync.nil?
          kwargs = kwargs.key?(:include) || kwargs.key?(:exclude) ? kw.call : @revbuild || {}
          case flag
          when :build
            op = OptionPartition.new(opts, VAL_GIT[:revbuild].map { |key| "#{key}=b?" }, project: self)
            op.clear(append: true)
            args = op.to_a
          else
            args = parse_env('GIT_OPTIONS')
                   .grep(/\A--#{Regexp.union(*VAL_GIT[:revbuild])}/)
                   .concat(VAL_GIT[:revbuild].map { |key| option(key, prefix: 'git') { |val| basic_option(key, val) } })
                   .compact
            OptionPartition.uniq!(args)
          end
          if (cur = workspace.rev_entry(name)) && cur['revision'] == sha && !env('REVBUILD_FORCE')
            files = status_digest(*args, **kwargs)
            if cur['files'].size == files.size && cur['files'].find { |key, val| files[key] != val }.nil?
              workspace.rev_timeutc(name, 'build') unless (since = workspace.rev_timesince(name, 'build'))
              puts log_message(['revbuild', 'no changes'], subject: name, hint: ("#{since} ago" if since)) if stdout?
              return
            end
          end
          start = time_epoch
          build(*@output, sync: sync, from: :'git:revbuild')
        rescue StandardError => e
          print_error(e, pass: true)
        else
          print_status('revbuild', subject: name, start: start, from: :completed)
          workspace.rev_write(name, { 'revision' => sha, 'files' => status_digest(*args, **kwargs) },
                              sync: sync, utc: 'build')
        end

        def reset(flag, opts = [], refs: nil, ref: nil, mode: nil, commit: nil)
          cmd, opts = git_session('reset', opts: opts)
          case flag
          when :commit, :index
            op = OptionPartition.new(opts, OPT_GIT[:reset] + VAL_GIT[:reset] + OPT_GIT[:log][:diff_context], cmd,
                                     project: self, no: OPT_GIT[:no][:reset], first: (matchpathspec if flag == :index))
            if flag == :commit
              op.append(commit)
                .clear(pass: false)
              ref = false
            else
              refs = op.extras
            end
          when :mode
            return unless VAL_GIT[:reset].include?(mode)

            cmd << "--#{mode}"
            if mode == 'mixed'
              cmd << '-N' if option('n')
              cmd << '--no-refresh' if option('refresh', equals: '0')
            end
          when :patch
            cmd << '--patch'
          when :undo
            cmd << '--hard HEAD@{1}'
            ref = false
          end
          unless ref == false
            append_commit(ref, head: true)
            append_pathspec(refs, pass: false) if refs
          end
          source
        end

        def checkout(flag, opts = [], branch: nil, origin: nil, create: nil, commit: nil, detach: nil, merge: false)
          cmd, opts = git_session('checkout', opts: opts)
          append_option 'f', 'force', 'merge'
          case flag
          when :branch
            cmd << '--detach' if detach == 'd' || option('detach')
            append_option('track', equals: true)
            cmd << (create ? quote_option(create, branch) : branch) << commit
          when :track
            cmd << quote_option(branch.delete_prefix!('^') ? 'B' : 'b', branch) if branch
            cmd << '--track' << shell_quote(origin)
          when :detach
            cmd << '-m' if merge
            cmd << '--detach' << commit
          else
            list = OPT_GIT[:checkout] + OPT_GIT[:log][:diff_context]
            op = OptionPartition.new(opts, list, cmd, project: self, no: OPT_GIT[:no][:checkout],
                                                      first: (matchpathspec if flag == :path))
            if flag == :path
              append_head
              append_pathspec(op.extras, pass: false)
              return success?(source)
            end
            op.append(commit)
              .clear(pass: false)
          end
          source
        end

        def tag(flag, opts = [], refs: [], message: nil, commit: nil, remote: nil)
          cmd, opts = git_session('tag', opts: opts)
          case flag
          when :add, :sign
            if flag == :sign || option('sign')
              cmd << '--sign'
            elsif !session_arg?('s', 'sign', 'u', 'local-user')
              cmd << '--annotate'
            end
            cmd << '--force' if option('f', 'force')
            if !commit && message && (sha = commithash(message))
              commit = sha
              message = nil
            end
            append_message message
            append_value refs
            append_head commit
          when :delete
            cmd << '--delete'
            append_value refs
          else
            op = OptionPartition.new(opts, OPT_GIT[:tag], cmd << '--list', project: self, no: OPT_GIT[:no][:tag])
            out, banner, from = source(io: true)
            print_item banner
            ret = write_lines(out, grep: op.extras)
            list_result(ret, 'tags', grep: op.extras, from: from)
            return
          end
          remote ||= option('remote')
          source.tap { |ret| git_spawn('push', ('-d' if flag == :delete), remote, *refs.quote!) if ret && remote }
        end

        def log!(flag, opts = [], range: [], index: [], grep: [])
          cmd, opts = git_session('log', opts: opts)
          op = OptionPartition.new(opts, collect_hash(OPT_GIT[:log]), cmd, project: self,
                                                                           no: collect_hash(OPT_GIT[:no][:log]),
                                                                           first: matchpathspec)
          case flag
          when :between, :contain
            op.add_quote(range.join(flag == :between ? '..' : '...'))
          when :grep
            op.merge(grep.map { |val| quote_option('grep', val) })
          else
            op.merge(index)
          end
          append_nocolor
          append_pathspec op.extras
          source(exception: false)
        end

        def diff(flag, opts = [], refs: [], branch: nil, range: [], index: [])
          cmd, opts = git_session('diff', opts: opts)
          op = OptionPartition.new(opts,
                                   collect_hash(OPT_GIT[:diff]) + OPT_GIT[:log][:diff] + OPT_GIT[:log][:diff_context],
                                   cmd,
                                   project: self, no: OPT_GIT[:no][:log][:diff],
                                   first: (matchpathspec unless flag == :files))
          case flag
          when :files, :view, :between, :contain
            op.delete('--cached')
          end
          append_nocolor
          if flag == :files
            op << '--no-index'
            patch = refs.pop
            append_pathspec(refs, parent: true)
            if patch
              patch = basepath patch
              exit 1 if patch.exist? && !confirm_basic('Overwrite?', patch)
              op << '>' << shell_quote(patch)
              source(banner: false)
              puts patch.read if patch.exist? && (stdin? || verbose?)
              return
            end
          else
            op << '--merge-base' if option('merge-base')
            case flag
            when :view
              op.merge(range)
            when :between, :contain
              op.delete('--merge-base')
              op.add_quote(range.join(flag == :between ? '..' : '...'))
            else
              op.add_quote(branch) if branch
              if !index.empty?
                if op.arg?('cached')
                  raise_error "single commit: #{index.join(', ')}", hint: 'cached' unless index.size == 1
                  op << index.first
                else
                  op.merge(index)
                end
              elsif (n = option('index', ignore: false))
                op << "HEAD~#{n}"
              end
            end
            append_pathspec op.extras
          end
          source(exception: op.arg?('exit-code'))
        end

        def commit(flag, opts = [], refs: [], ref: nil, squash: nil, pick: nil, message: nil, pass: false)
          fixup = flag == :fixup
          amend = flag.match?(/^amend/) && !fixup
          unless flag == :add || pick == 'reword'
            pathspec = if flag == :all || ((fixup || amend) && refs.size == 1 && refs.first == '*')
                         '--all'
                       elsif (refs = projectmap(refs)).empty?
                         raise_error 'no qualified pathspec'
                       else
                         "-- #{refs.join(' ')}"
                       end
          end
          if fixup
            ret = source(git_session('commit', basic_option('fixup', pick ? "#{pick}:#{ref}" : ref), pathspec))
            source git_output('rebase --autosquash', squash) if ret && squash.is_a?(String)
            return ret
          end
          message ||= messageopt
          unless message || amend
            return if pass

            message = readline('Enter message', force: true)
          end
          branch = nil
          origin = nil
          upstream = nil
          cmd, opts = git_session('add', opts: opts)
          op = OptionPartition.new(opts, OPT_GIT[:add] + OPT_GIT[:log][:diff_context], cmd,
                                   project: self, no: OPT_GIT[:no][:add], first: matchpathspec)
          op << '--verbose' unless silent?
          format = '%(if)%(HEAD)%(then)%(refname:short)...%(upstream:short)...%(upstream:track)%(end)'
          git_spawn 'fetch --no-tags --quiet'
          foreachref('heads', format: format).each do |line|
            next if (line = line.chomp).empty?

            branch, origin, hint = line.split('...')
            if hint && !hint.match?(/^\[(\D+0,\D+0)\]$/)
              raise_error 'work tree is not usable', hint: hint[1..-2]
            elsif (!origin || origin.empty?) && !dryrun?
              return nil if pass

              unless (origin = option('upstream', prefix: 'git', ignore: false))
                if (origin = choice_refs('Choose an upstream', 'remotes', attempts: 1, force: false))
                  git_spawn 'branch', quote_option('set-upstream-to', origin)
                  break
                end
                origin = readline('Enter an upstream', force: true)
              end
              upstream = true
            end
            break
          end
          if pathspec
            op << pathspec
          else
            append_pathspec op.extras
          end
          co = git_session('commit', options: false)
          pu = git_output 'push'
          co << '--amend' if amend
          pu << '--set-upstream' if upstream
          if dryrun?
            op.adjoin('--dry-run')
            co << '--dry-run'
            pu << '--dry-run'
          end
          if message
            append_message message
          elsif flag == :'amend-orig' || option('edit', equals: '0')
            co << '--no-edit'
          end
          pu << '--force-with-lease' if amend
          pu.merge(repotrack(origin, branch))
          adding = git_spawn 'diff --name-only --no-color'
          source op
          cached = git_spawn 'diff --cached --name-only --no-color'
          if amend || !cached.empty? || dryrun?
            if adding.empty? && !cached.empty? && banner?
              puts(cached.lines(chomp: true).map! { |val| "cached #{shell_quote(val)}" })
            end
            source co
            source pu
          else
            if banner?
              puts 'Nothing to commit'
            elsif stdout?
              puts log_message('nothing to commit', subject: name, hint: flag)
            end
            exit 1
          end
        end

        def merge(flag, opts = [], command: nil, branch: nil)
          cmd, opts = git_session('merge', opts: opts)
          display = false
          case flag
          when :commit, :'no-commit'
            op = OptionPartition.new(opts, OPT_GIT[:merge], cmd, project: self, no: OPT_GIT[:no][:merge])
            op << "--#{flag}"
            op.delim
            if branch
              op << branch
              op.clear(pass: false)
            else
              raise_error ArgumentError, 'no branch/commit' if op.empty?
              append_commit(*op.extras)
            end
          else
            unless gitpath('MERGE_HEAD').exist?
              puts log_message('no merge in progress', subject: name, hint: command) if stdout?
              exit 1
            end
            return unless VAL_GIT[:merge][:send].include?(command)

            cmd << "--#{command}"
            display = command == 'abort'
          end
          success?(source, display)
        end

        def branch(flag = nil, opts = [], refs: [], ref: nil, target: nil, remote: nil)
          cmd, opts = git_session('branch', opts: opts)
          stdout = false
          case flag
          when :create
            option('track', ignore: false) do |val|
              cmd << case val
                     when '0', 'false'
                       '--no-track'
                     when 'direct', 'inherit'
                       basic_option 'track', val
                     else
                       '--track'
                     end
            end
            cmd << '--force' if option('f', 'force')
            cmd << shell_quote(target)
            cmd << shell_quote(ref) if ref
          when :track
            raise_error 'invalid upstream', hint: ref unless ref.include?('/')
            if ref.delete_prefix!('^')
              cmd << '--unset-upstream' << shell_quote(ref)
              remote = false
              stdout = true
            else
              cmd << quote_option('set-upstream-to', ref)
              cmd << shell_quote(target) if target
            end
          when :delete
            remote&.each { |val| source git_output('push --delete', *val.split('/', 2).quote!) }
            force, list = refs.partition { |val| val.start_with?(/[~^]/) }
            force.each do |val|
              r = '-r' if val.delete!('~')
              source git_output('branch', val.delete!('^') ? '-D' : '-d', r, shell_quote(val))
            end
            return if list.empty?

            cmd << '-d'
            append_value list
            remote = nil
          when :move, :copy
            s = +"-#{flag.to_s[0]}"
            s.upcase! if option('f', 'force')
            cmd << s
            cmd.merge(refs.compact.quote!)
            stdout = true
          when :current
            cmd << '--show-current'
            source(banner: verbose?, stdout: true)
            return
          when :list
            op = OptionPartition.new(opts, OPT_GIT[:branch], cmd << '--list',
                                     project: self, no: OPT_GIT[:no][:branch], single: /\Av+\z/)
            op.each { |val| op.add_quote(val) }
            out, banner, from = source(io: true)
            print_item banner
            ret = write_lines(out, sub: [
              opt_style(color(:green), /^(\*\s+)(\S+)(.*)$/, 2),
              opt_style(color(:red), %r{^(\s*)(remotes/\S+)(.*)$}, 2)
            ])
            list_result(ret, 'branches', from: from)
            return
          else
            if (head = git_spawn('rev-parse --abbrev-ref HEAD').chomp).empty?
              ret = 0
            else
              git_spawn 'fetch --all --prune --quiet' if option('sync')
              cmd << '-vv --no-abbrev --list'
              out, banner, from = source(io: true)
              first = workspace.size > 1
              grep = first ? [/^\*\s+#{Regexp.escape(head)}\s/] : []
              ret = write_lines(out, grep: grep, banner: banner, first: first) do |line, index|
                next line if stdin?

                data = line.sub(/^\*?\s+/, '').split(/\s+/, 3)
                a = sub_style(data[0], theme[:inline], styles: (:underline if !first && line.start_with?('*')))
                b = commitstyle data[1]
                r = /\A(?:\[((?~\]\s))\]\s)?(.+)\z/m.match(data[2])
                if (r1 = r[1]) && r1 =~ /^(.+):(?: ([a-z]+) (\d+),)? ([a-z]+) (\d+)$/
                  write = ->(s1, s2) { "#{s1.capitalize.rjust(7)}: #{sub_style(s2, theme[:warn])}" }
                  r1 = $1
                  r2 = $2 && write.call($2, $3)
                  r3 = write.call($4, $5)
                end
                r1 = nil if r1 == "origin/#{data[0]}"
                ["#{"\n" unless index == 0} Branch: #{a.subhint(r1)}", r2, r3, " Commit: #{b}", "Message: #{r[2]}"]
                  .compact
                  .join("\n")
              end
              on :last, from
            end
            print_error(name, 'no ref found', subject: 'branch', hint: 'head', pass: true) if ret == 0
            return
          end
          return unless success?(source(stdout: stdout), !ref && flag == :create) && !ref && remote && target

          source git_output('push -u', shell_quote(ref.split('/', 2).first), shell_quote(target))
        end

        def switch(flag, opts = [], branch: nil, commit: nil, track: nil)
          cmd, opts = git_session('switch', opts: opts)
          cmd << '--force' if option('f', 'force')
          if flag == :branch
            OptionPartition.new(opts, OPT_GIT[:switch], cmd, project: self, no: OPT_GIT[:no][:switch])
                           .add_quote(branch)
          else
            case flag
            when :create
              cmd << quote_option(branch.delete_prefix!('^') ? 'C' : 'c', branch)
              cmd << case (track ||= option('track', ignore: false))&.downcase
                     when 'n', '0', 'false'
                       '--no-track'
                     when 'y', '1', 'true'
                       '--track'
                     when 'direct', 'inherit'
                       basic_option 'track', track
                     end
            when :detach
              cmd << '--detach'
            end
            append_head commit
          end
          source
        end

        def submodule(flag, opts = [], branch: nil, path: nil, url: nil)
          cmd, opts = git_session('submodule', opts: opts)
          op = OptionPartition.new(opts, OPT_GIT[:submodule].fetch(flag, []), cmd, project: self)
          case flag
          when :branch, :url
            op.adjoin("set-#{flag}")
            op.add_quote(branch, '--', path, url)
          else
            op.adjoin(flag)
            op << '--recursive' if option('r', 'recursive')
            op.splice(path: true)
          end
          source.tap { |ret| success?(ret, flag == :branch) }
        end

        def restore(flag, opts = [], commit: nil, files: nil)
          cmd, opts = git_session('restore', shell_option(flag, commit, escape: false, force: false), opts: opts)
          op = OptionPartition.new(opts,
                                   OPT_GIT[:restore] + OPT_GIT[:log][:diff_context],
                                   cmd,
                                   project: self, no: OPT_GIT[:no][:restore], first: matchpathspec)
          append_pathspec(op.extras + (files || []), pass: false)
          source(sync: false, stderr: true)
        end

        def show(flag, opts = [], format: nil, files: [])
          cmd, opts = git_session('show', opts: opts)
          case flag
          when :textconv
            cmd << '--textconv'
            append_value(files.flat_map { |val| Dir[val] }
                              .select { |val| projectpath?(val) }
                              .map! { |val| shell_quote("HEAD:#{val}") })
            source(banner: false)
            return
          when :oneline
            format = flag.to_s
          end
          case format
          when 'oneline', 'short', 'medium', 'full', 'fuller', 'reference', 'email', 'raw'
            cmd << basic_option('format', format)
          when /^t?format:|%/
            cmd << quote_option('pretty', format)
          else
            opts << format if format
          end
          list = OPT_GIT[:show] + OPT_GIT[:diff][:show] + OPT_GIT[:log][:diff] + OPT_GIT[:log][:diff_context]
          op = OptionPartition.new(opts, list, cmd,
                                   project: self,
                                   no: OPT_GIT[:no][:show] + collect_hash(OPT_GIT[:no][:log], pass: [:base]))
          op.append(delim: true)
          source(exception: false, banner: flag != :oneline)
        end

        def rev_parse(flag, opts = [], ref: nil, size: nil)
          cmd, opts = git_session('rev-parse', opts: opts)
          case flag
          when :commit
            cmd << (size.to_i.zero? ? '--verify' : basic_option('short', [size.to_i, 5].max))
            append_commit(ref, head: true)
          when :branch
            cmd << '--abbrev-ref'
            append_commit(ref, head: true)
          when :output
            if opts.delete('sq-quote')
              cmd << '--sq-quote'
              args = true
            end
            OptionPartition.new(opts, OPT_GIT[:rev_parse], cmd, project: self, no: OPT_GIT[:no][:rev_parse], args: args)
                           .append(escape: args)
          end
          source(banner: verbose?)
        end

        def ls_remote(flag, opts = [], remote: nil)
          cmd, opts = git_session('ls-remote --refs', opts: opts)
          cmd << "--#{flag}" unless flag == :remote
          op = OptionPartition.new(opts, OPT_GIT[:ls_remote], cmd, project: self)
          op.add_quote(remote) if remote
          out, banner, from = source(io: true)
          print_item banner
          ret = write_lines(out, grep: op.extras, prefix: "refs/#{flag}/")
          list_result(ret, flag.to_s, grep: op.extras, from: from)
        end

        def ls_files(flag, opts = [])
          cmd, opts = git_session("ls-files --#{flag}", opts: opts)
          op = OptionPartition.new(opts, OPT_GIT[:ls_files], cmd, project: self)
          op.splice(path: true, pattern: true)
          out, banner, from = source(io: true)
          print_item banner
          ret = write_lines(out, grep: op.extras)
          list_result(ret, 'files', grep: op.extras, from: from)
        end

        def git(flag, opts = [])
          cmd, opts = git_session(flag, opts: opts)
          list = OPT_GIT[:git].fetch(flag, []) + OPT_GIT.fetch(flag, [])
          case flag
          when :add
            list.concat(OPT_GIT[:log][:diff_context])
          when :revert
            list.concat(VAL_GIT[:rebase][:send])
          end
          op = OptionPartition.new(opts, list, cmd, project: self, no: OPT_GIT[:no][flag], single: /\A\d+\z/,
                                                    first: case flag
                                                           when :blame, :revert then nil
                                                           else matchpathspec
                                                           end)
          case flag
          when :blame
            raise_error Errno::ENOENT, 'no file target' unless (n = op.index { |s| basepath(s).file? })
            op.append(basepath(op.remove_at(n)), delim: true)
              .clear
          when :revert
            if op.arg?(*VAL_GIT[:rebase][:send])
              op.clear
            elsif op.empty?
              raise_error 'no commit target'
            else
              append_commit(*op.extras)
            end
          when :add
            if flag == :add && !op.arg?('pathspec-from-file')
              grep, pathspec = op.partition { |val| OptionPartition.pattern?(val) }
              unless grep.empty? && !pathspec.empty?
                grep.map! { |val| Regexp.new(val[1..-2]) }
                files = []
                status_data.each do |a, b|
                  next if b.strip.empty? || (!grep.empty? && grep.none? { |pat| pat.match?(a) })

                  files << "#{sub_style(b, color(:red))} #{a}"
                end
                unless files.empty?
                  files = choice_index('Select files', files, multiple: true, trim: /^\S+\s/,
                                                              accept: [accept_y('Add?')])
                end
                op.swap(pathspec + files)
              end
            end
            return source(git_session('status -s'), banner: false) unless append_pathspec(op.extras)
            return success?(source) if flag == :add && !op.arg?('verbose')
          when :mv
            refs = projectmap op.extras
            raise_error 'no source/destination' unless refs.size > 1
            op.merge(refs)
          when :rm, :clean
            append_pathspec(op.extras, expect: flag == :rm)
          when :grep
            op.each do |val|
              if op.include?('--')
                op.add_path(val)
              elsif op.exist?(val, glob: true)
                op.delim
                  .add_path(val)
              else
                op.add_quote(val)
              end
            end
          end
          case flag
          when :revert, :mv, :rm
            source(sync: false, stderr: true)
          else
            source
          end
        end

        def clone?
          ref?(workspace.baseref) && workspace.git_clone?(path, name) ? 1 : false
        end

        def revbuild?
          build? && !!workspace.revfile
        end

        def enabled?(*, **kwargs)
          super || (kwargs[:base] == false && !!clone?)
        end

        private

        def source(cmd = @session, exception: true, io: false, sync: true, stdout: false, stderr: false, banner: true,
                   multiple: false, hint: nil, from: nil, send: :system, **kwargs)
          cmd = cmd.target if cmd.is_a?(OptionPartition)
          if io && banner == false
            from = nil
            banner = nil
          else
            if banner
              banner = nil unless banner? && !multiple
              args = true
            end
            if from == false
              from = nil
            elsif !from && cmd.respond_to?(:drop)
              from = cmd.drop(1).find { |val| val.match?(/\A[a-z]{1,2}[a-z-]*\z/) }
              from &&= :"git:#{from}"
            end
            banner &&= cmd.temp { |val| val.start_with?(/--(?:work-tree|git-dir)/) } if cmd.respond_to?(:temp)
          end
          cmd = session_done cmd
          log&.info cmd
          banner = if banner
                     format_banner(banner.is_a?(String) ? banner : cmd, hint: hint, strip: true)
                   end
          on :first, from
          begin
            if io
              return `#{cmd}` if stdout

              return args ? [IO.popen(cmd), banner || '', from] : IO.popen(cmd)
            elsif stdin? ? sync : stdout
              print_item banner unless multiple
              ret = `#{cmd}`.chomp
              raise(ret.empty? ? $?.to_s : ret) unless $?.success?

              if ret.empty?
                success?(true, !banner.nil?)
              else
                puts ret
              end
            elsif !kwargs[:sub] && (sync || (!exception && !stderr))
              print_item banner unless multiple
              ret = shell(cmd, name: send, exception: exception)
            else
              require 'open3'
              if stderr
                Open3.popen3(cmd) do |_, out, err|
                  n = write_lines(out, banner: banner, pass: true, **kwargs)
                  if n == 0
                    n = write_lines(err, banner: banner)
                    success?(n == 0, n == 0 && !banner.nil?)
                  else
                    write_lines(err, loglevel: Logger::DEBUG)
                  end
                end
              else
                Open3.popen2e(cmd) { |_, out| write_lines(out, banner: banner, **kwargs) }
              end
            end
          rescue StandardError => e
            on_error(e, from, pass: true)
            nil
          else
            on :last, from
            ret
          end
        end

        def write_lines(data, grep: [], prefix: nil, sub: nil, banner: nil, loglevel: nil, pass: false, first: false)
          grep = (matchmap(grep, prefix) unless grep.empty?)
          sub = (as_a sub unless stdin?)
          ret = 0
          out = []
          data.each do |line|
            next if grep&.none? { |pat| pat.match?(line) }
            next if block_given? && !(line = yield(line, ret))

            if loglevel
              log&.add loglevel, line
            else
              sub&.each { |h| sub_style!(line, **h) }
              if banner
                out << line
              else
                puts line
              end
            end
            ret += 1
            break if first
          end
          print_item banner, out if banner && (ret > 0 || (!pass && !first))
          ret
        end

        def list_result(size, type, action: 'found', grep: [], from: nil)
          if size == 0
            puts empty_status("No #{type} were #{action}", 'grep', grep.join(', '))
          elsif stdout?
            styles = theme.fetch(:banner, []).reject { |s| s.to_s.end_with?('!') }
            styles << :bold if styles.size <= 1
            puts print_footer("#{size} #{size == 1 ? type.sub(/(?:(?<!l)e)?s\z/, '') : type}",
                              sub: opt_style(styles, /^(\d+)(.+)$/))
          end
          on :last, from
        end

        def choice_refs(msg, *type, format: nil, sort: '-creatordate', count: true, short: true, **kwargs)
          type << 'heads' if type.empty?
          unless format
            format = +"%(refname#{':short' if short})"
            if type.include?('heads') || type.include?('tags')
              format += '%(if)%(HEAD)%(then) *%(end)'
              trim = /\s+\*\z/
            end
          end
          args = []
          args << quote_option('sort', sort) if sort
          args << basic_option('count', env('GIT_COUNT', ARG[:CHOICE])) if count
          choice_index(msg, foreachref(type, *args, format: format), trim: trim, **kwargs)
        end

        def choice_commit(count: true, reflog: true, force: true, **kwargs)
          kwargs[:attempts] ||= 1 unless force
          cmd = git_output(reflog && env('GIT_REFLOG') ? 'reflog' : 'log')
          cmd << quote_option('format', "#{commitstyle('%h')} %s")
          cmd << basic_option('max-count', env('GIT_COUNT', ARG[:CHOICE])) if count
          ret = choice_index('Choose a commit', git_spawn(cmd, stdout: false), column: /^(\S+)/, force: force, **kwargs)
          case ret
          when Array
            ret.map!(&:stripstyle)
          when String
            ret.stripstyle
          else
            ret
          end
        end

        def choice_remote(force: false, **kwargs)
          kwargs[:attempts] ||= 1 unless force
          choice_index('Select a remote', git_spawn('remote', stdout: false), force: force, **kwargs)
        end

        def status_digest(*args, algorithm: nil, **kwargs)
          require 'digest'
          algorithm ||= Digest::SHA256
          glob = kwargs.fetch(:include, [])
          pass = kwargs.fetch(:exclude, [])
          ret = {}
          status_data(*args).each do |file,|
            next if !glob.empty? && glob.none? { |val| File.fnmatch?(val, file, File::FNM_DOTMATCH) }
            next if pass.any? { |val| File.fnmatch?(val, file, File::FNM_DOTMATCH) }

            ret[file] = algorithm.hexdigest(File.read(basepath(file)))
          end
          ret
        end

        def status_data(*args)
          ret = []
          git_spawn('status -z -uall', *args).split("\x0").each do |line|
            next unless line =~ /^(.)(.) (.+)$/

            ret << [$3, $2, $1]
          end
          ret
        end

        def append_pull(opts, list, flag:, from:, target: @session, no: nil, remote: nil)
          target << '--force' if option('f', 'force', target: target)
          append_submodules(target: target, from: from)
          return if !remote && opts.empty?

          refspec = []
          op = OptionPartition.new(opts, remote ? list + ['refspec=v'] : list, target, project: self, no: no)
          op.each do |opt|
            if opt =~ op.values
              case $1
              when 'rebase'
                op << basic_option($1, $2) if VAL_GIT[:rebase][:value].include?($2)
              when 'shallow-since'
                require 'date'
                op.append?($1) { Date.parse($2).strftime('%F %T') }
              when 'recurse-submodules'
                op.append?($1, $2, type: :basic)
              when 'refspec'
                refspec << shell_quote($2)
              end
            elsif op.arg?('multiple')
              op.found << opt
            else
              op.errors << opt
            end
          end
          op << '--verbose' if (flag || from == :fetch) && stdout? && !op.arg?('quiet')
          if remote
            op.append(remote, delim: true)
            if (val = option('refspec', target: target, strict: true))
              op.append(*split_escape(val))
            else
              op.merge(refspec)
            end
            op.delete('--all')
          elsif op.arg?('multiple')
            op.add_quote(*op.found)
            return
          elsif option('all')
            op << '--all'
          end
          op.clear(errors: true, subject: flag) if flag
        end

        def append_commit(*val, target: @session, head: false)
          val.compact!
          if !val.empty?
            val.each { |ref| target << (commithash(ref) || shell_quote(ref)) }
          elsif head
            target << (append_head(target: target) || 'HEAD')
          end
        end

        def append_pathspec(files = [], target: @session, expect: false, parent: false, pass: true)
          if session_arg?('pathspec-from-file', target: target)
            option_clear files
            true
          else
            option('pathspec', target: target) { |val| files = split_escape(val) } if files.empty?
            files = projectmap(files, parent: parent, pass: pass)
            if !files.empty?
              target << '--' << files.join(' ')
              true
            elsif expect
              raise_error(parent ? 'pathspec not present' : 'pathspec not within worktree')
            else
              false
            end
          end
        end

        def append_message(val = nil, target: @session)
          val = messageopt if val.to_s.empty?
          return unless val

          target << quote_option('message', val)
        end

        def append_head(val = nil, target: @session)
          return target << shell_quote(val) if val

          append_first('head', 'tree-ish', 'object', target: target, flag: false, ignore: false)
        end

        def append_submodules(target: @session, from: nil)
          option('recurse-submodules', target: target, ignore: false) do |val|
            if from == :clone
              case val
              when '0', 'false'
                target << '--no-recurse-submodules'
              when '1', 'true'
                target << '--recurse-submodules'
              else
                projectmap(split_escape(val)).each { |path| target << basic_option('recurse-submodules', path) }
              end
            else
              target << case val
                        when 'no', '0', 'false'
                          '--no-recurse-submodules'
                        when 'yes', 'on-demand'
                          "--recurse-submodules=#{val}"
                        else
                          '--recurse-submodules'
                        end
            end
          end
        end

        def foreachref(path, *args, format: nil)
          path = Array(path).map! { |val| "refs/#{val}" }
          format &&= quote_option('format', format)
          ret = git_spawn('for-each-ref', format, *args, *path, stdout: workspace.windows?)
          ret.is_a?(String) ? ret.lines : ret
        end

        def git_session(*cmd, opts: nil, worktree: true, **kwargs)
          dir = worktree ? [quote_option('work-tree', path), quote_option('git-dir', gitpath)] : []
          return session('git', *dir, *cmd, **kwargs) unless opts

          op = OptionPartition.new(opts, OPT_GIT[:common], dir, project: self)
          [session('git', *op.to_a, *cmd, **kwargs), op.extras]
        end

        def git_output(*cmd, **kwargs)
          git_session(*cmd, main: false, options: false, **kwargs)
        end

        def git_spawn(*cmd, exception: true, io: true, sync: true, stdout: true, banner: false, **kwargs)
          kwargs[:send] = if sync
                            :system
                          else
                            exception = false
                            io = false
                            stdout = false
                            :spawn
                          end
          source(cmd.first.is_a?(Set) ? cmd.first : git_output(*cmd), exception: exception, io: io, sync: sync,
                                                                      stdout: stdout, banner: banner, **kwargs)
        end

        def dryrun?(*, target: @session, prefix: target&.first)
          Array(target).include?('--dry-run') || !option('dry-run', target: target, prefix: prefix).nil?
        end

        def quiet?(*, target: @session, **)
          return false unless target

          target.include?('--quiet') || (target.include?('-q') && target.first.stripext == 'git')
        end

        def gitpath(*args)
          basepath('.git', *args)
        end

        def repotrack(origin, branch, quote: true)
          unless origin && branch && (i = origin.index('/'))
            raise_error(ArgumentError, "missing #{origin ? 'branch' : 'remote'} name", hint: origin)
          end
          branch = "#{branch}:#{origin[i.succ..-1]}" unless origin.end_with?("/#{branch}")
          [origin[0..i.pred], branch].tap { |ret| ret.quote! if quote }
        end

        def commithash(val)
          val[/\A:(\h{5,40})\z/, 1]
        end

        def commithead(val)
          return val unless (s = matchhead(val))

          s.start_with?(/\d/) ? "@~#{s}" : "@#{s}"
        end

        def commitstyle(val)
          sub_style(val, theme[:extra] || color(:yellow))
        end

        def matchhead(val)
          val =~ /^(?:(?:HEAD|@)([~^]\d*)?|H(\d+))$/ ? $2 || $1 || '' : nil
        end

        def matchpathspec
          [/\A[^a-z\d-]+/i, %r{\A[^=\\/*]*[\\/*]}, /\A--\z/]
        end

        def messageopt
          option('message', 'm', prefix: 'git', ignore: false)
        end

        def threadargs
          { stderr: true, exception: exception || !workspace.series.multiple? }
        end
      end

      Application.implement Git
    end

    class << self
      include Common

      def resolve(*args, base: '..')
        values = __get__(:project).values
        ret = args.map do |id|
          if id.is_a?(Symbol)
            project id
          else
            values.find { |proj| proj.name == id } ||
            values.find { |proj| proj.project == id } ||
            values.find { |proj| proj.path.to_s == File.expand_path(id, base) }
          end
        end
        ret.size == 1 ? ret.first : ret
      end

      def expect(name)
        ret = project name
        return ret if ret&.path&.directory? && !ret.path.empty?

        raise NoMethodError, "project is not initialized (#{name})"
      end

      def project?(name)
        !!project(name)&.enabled?
      end

      private

      def project(name)
        __get__(:project)[name.to_s]
      end
    end

    module Repo
      REPO_URL = 'https://storage.googleapis.com/git-repo-downloads/repo'
      private_constant :REPO_URL

      attr_reader :manifest_url, :manifest

      def repo(url, manifest = 'latest', install: nil, run: nil, script: nil, args: nil, dev: nil, prod: nil,
               ref: @ref, group: @group)
        @home = if (val = env('REPO_HOME'))
                  path = Pathname.new(val)
                  if main == path.basename.to_s
                    @root = path.parent
                    if path.exist?
                      @root = nil unless path.directory?
                    elsif !@root.exist?
                      @root.mkpath
                    elsif !repo_install? && !repo_confirm
                      @root = nil
                    end
                    raise_error Errno::EEXIST, path.cleanpath, hint: 'REPO_HOME' unless @root
                  end
                  path.realdirpath
                elsif (val = env('REPO_ROOT'))
                  @root = Pathname.new(val).realdirpath
                  if !@root.exist?
                    @root.mkpath
                  elsif !repo_install?(parent: true) && !repo_confirm
                    raise_error Errno::EEXIST, @root, hint: 'REPO_ROOT'
                  end
                  @root.join(main).realdirpath
                elsif repo_install?(parent: true) && (!@home.exist? || @root + main == @home)
                  @home
                elsif repo_install?(@home)
                  @home + main
                else
                  (path = pwd) == @home || !repo_install?(path) ? @home : path + main
                end
        @root = @home.parent
        @manifest_url = url
        @manifest = manifest
        data = scriptobj
        if repo?
          sc, ru = env('REPO_BUILD', '').split(',', 2).map!(&:strip)
          if script
            data[:script] = if sc.to_s.empty?
                              script
                            else
                              data[:env][:script] = true
                              case sc
                              when 'verbose'
                                @verbose = 1
                                if script.is_a?(Array)
                                  script[0] = task_join script[0], 'verbose'
                                  script
                                else
                                  task_join script, 'verbose'
                                end
                              when 'silent'
                                @verbose = false
                                @warning = false
                                script
                              else
                                sc
                              end
                            end
            data[:args] = (val = env('REPO_SCRIPT')) ? shell_split(val, join: true) : args
            data[:global][:script] = true
          else
            ru ||= sc
          end
          if run
            data[:run] = if ru.to_s.empty?
                           run
                         else
                           data[:env][:run] = true
                           ru
                         end
            data[:global][:run] = true
          end
          data[:dev] = env_match 'REPO_DEV', dev
          data[:prod] = env_match 'REPO_PROD', prod
          if (val = env('REPO_GROUP'))
            script_set(data, group: val.split(','))
            found = true
          end
          if (val = env('REPO_REF'))
            script_set(data, ref: val.split(','))
            found = true
          end
          script_set(data, group: group, ref: ref) unless found
          @warning = env_match('REPO_WARN', @warning && !root?(@root, pass: ['.repo'])) != false
          @extensions << :__repo__
        elsif script || run
          if script
            data[:script] = script
            data[:args] = args
          end
          data[:run] = run if run
          data[:dev] = dev
          data[:prod] = prod
          script_set(data, group: group, ref: ref)
        end
        @repo_bin = install.is_a?(String) ? @root + install : @root if install
        self
      end

      private

      def __repo__(**kwargs)
        kwargs.delete(:parallel) if env('REPO_SYNC', ignore: '0')

        namespace task_name('repo') do |ns|
          path = ns.scope.path
          branch = env('REPO_MANIFEST') || repo_manifest
          target = branch || manifest
          stage = nil
          opts = %w[force rebase detach submodules fail no-update gc]
          desc = lambda do |val, alt = nil|
            if (ver = branch || alt)
              val = val.sub('{0}', "opts*=#{opts.join(',')}")
              task_desc(path, val, ver)
            else
              task_desc 'inactive'
            end
          end

          desc.call('all[{0}]')
          task 'all' do |_, args|
            stage ||= 'all'
            ns['sync'].invoke(*args.to_a)
            next if (n = env('REPO_STAGE')) == '1'

            select do |proj|
              next unless proj.enabled?(proj.workspace.baseref) && proj.global

              proj.depend(sync: true) if proj.depend?
              next if n == '2'

              proj.build?
            end
            .each do |proj|
              proj.build(sync: true)
              next if n == '3'
              next unless proj.copy? && (proj.dev? || n == '4')

              if (ws = proj.workspace).task_defined?(target = task_join(proj.name, 'copy'))
                task_invoke(target, **ws.invokeargs)
              else
                proj.copy
              end
            end
          end

          desc.call("init[manifest?=#{target},groups?,{0}]", target)
          task 'init' do |_, args|
            args = args.to_a
            u = env('REPO_GIT') || manifest_url
            m = args.first && !opts.include?(args.first) ? args.shift : target
            g = (args.shift if args.first && !opts.include?(args.first))
            g = case (val = env('REPO_GROUPS'))
                when '', NilClass
                  g
                when '0', 'false'
                  nil
                else
                  val
                end
            stage = 'init'
            opts = repo_opts "-u #{u}", "-m #{m}.xml"
            opts << "-g #{g}" if g
            opts << '--submodules' if repo_submodules?(args.include?('submodules'))
            repo_run "#{repo_bin} init #{opts.uniq.join(' ')}"
            next if env('REPO_STAGE', equals: '0')

            ns['all'].invoke(*args)
          end

          desc.call('sync[{0}]')
          task 'sync' do |t, args|
            opts = if stage == 'init'
                     []
                   else
                     raise_error 'repo not initialized' unless branch
                     repo_opts
                   end
            args.to_a.each do |val|
              case val
              when 'force'
                opts << '--force-checkout'
              when 'rebase', 'detach'
                opts << "--#{val}"
              when 'submodules'
                opts << '--fetch-submodules' if repo_submodules?(true)
              when 'fail'
                opts << '--fail-fast'
              when 'no-update'
                opts << '--no-manifest-update'
              when 'gc'
                opts << '--auto-gc'
              end
            end
            opts << "-j#{ENV.fetch('REPO_JOBS', Rake::CpuCounter.count)}" unless opts.any?(/^--?j(?:obs)?/)
            opts << '--fetch-submodules' if repo_submodules?
            begin
              repo_run("#{repo_bin} sync #{opts.uniq.join(' ')}", exception: opts.include?('--fail-fast'))
            rescue Errno::ENOENT => e
              emphasize(e, title: root)
              raise
            rescue StandardError => e
              emphasize(e, title: "rake stash #{t.name}")
              raise
            end
          end

          series.sync.push(
            task_join(path, 'all'),
            task_join(path, 'init'),
            task_join(path, 'sync')
          )
        end
      end

      def repo_manifest(path = root)
        return unless (file = Pathname.new(path).join('.repo/manifest.xml')).exist?

        require 'rexml/document'
        doc = REXML::Document.new(file.read)
        doc.elements['manifest/include'].attributes['name']&.sub('.xml', '')
      end

      def repo_confirm
        return false unless root.directory?

        path = sub_style root, theme[:inline]
        @repo_override = case env('REPO_Y')
                         when '0', 'false'
                           false
                         when '1', 'true'
                           true
                         else
                           Common::Prompt.confirm(
                             "#{log_title(:warn)} \"#{path}\" is not empty. Continue with installation?", 'N',
                             force: true, timeout: env('REPO_TIMEOUT').to_i.yield_self { |n| n > 0 ? n : 15 }
                           )
                         end
      end

      def repo_run(cmd, exception: false)
        puts log_message(cmd, subject: main, hint: root) if verbose
        Common::System.shell(cmd, chdir: root, exception: exception)
      end

      def repo_bin
        return Common::Shell.shell_bin('repo') unless @repo_bin

        @repo_bin.join('repo').tap do |bin|
          next if bin.file?

          require 'open-uri'
          puts log_message('Installing repo...', subject: main, hint: @repo_bin) if verbose
          URI.open(REPO_URL) do |url|
            @repo_bin.mkpath
            File.open(bin, 'wb') do |f|
              f.write(url.read)
              f.chmod(0o755)
            end
          end
        end
      end

      def repo_opts(*args)
        return args unless (n = ARGV.index('--'))

        ARGV[n.succ..-1].concat(args)
      end

      def repo?
        return false unless manifest_url && !windows?

        repo_install? || @repo_override == true
      end

      def repo_submodules?(val = false)
        case (s = env('REPO_SUBMODULES'))
        when '0', 'false'
          false
        else
          s ? true : val
        end
      end

      def repo_install?(dir = root, parent: false)
        return true if root?(dir, pass: ['.repo']) || dir.join('.repo').directory?

        parent && root.children.none? { |ent| ent.directory? && ent.basename.to_s[0] != '.' && ent != home }
      end
    end

    Application.include Repo

    module Project
      class Node < Git
        OPT_NPM = {
          common: %w[dry-run=!? loglevel=b include-workspace-root=!? workspaces=!? w|workspace=v].freeze,
          install: %w[package-lock-only=!? prefer-dedupe=!? E|save-exact=!? before=q cpu=b libc=b os=b].freeze,
          install_a: %w[audit=! bin-links=! foreground-scripts=!? fund=! ignore-scripts=!? install-links=!?
                        package-lock=! strict-peer-deps=!? include=b install-strategy=b omit=b].freeze,
          install_b: %w[no-save B|save-bundle D|save-dev O|save-optional save-peer P|save-prod g|global=!?
                        S|save=!?].freeze,
          run: %w[foreground-scripts=!? if-present=!? ignore-scripts=!? script-shell=p].freeze,
          exec: %w[c|call=q package=b].freeze,
          pack: %w[ignore-scripts=!? json=!? pack-destination=p].freeze,
          rebuild: %w[bin-links=! foreground-scripts=!? global=!? ignore-scripts=!? install-links=!?].freeze,
          no: {
            install: %w[audit bin-links fund package-lock].freeze
          }.freeze
        }.freeze
        OPT_PNPM = {
          common: %w[aggregate-output color ignore-workspace-root-check no-color stream use-stderr C|dir=p loglevel=b
                     r|recursive w|workspace-root].freeze,
          common_cpu: %w[cpu=b libc=b os=b].freeze,
          common_filter: %w[fail-if-no-match changed-files-ignore-pattern=q filter=q filter-prod=q
                            test-pattern=q].freeze,
          install: %w[fix-lockfile force ignore-pnpmfile ignore-workspace lockfile-only merge-git-branch-lockfiles
                      optimistic-repeat-install no-hoist no-lockfile no-optional prefer-frozen-lockfile resolution-only
                      shamefully-hoist side-effects-cache side-effects-cache-readonly s|silent strict-peer-dependencies
                      use-running-store-server use-store-server child-concurrency=i hoist-pattern=q lockfile-dir=p
                      modules-dir=p network-concurrency=i package-import-method=b public-hoist-pattern=q
                      reporter=b].freeze,
          install_a: %w[dangerously-allow-all-builds global-dir ignore-scripts offline prefer-offline store-dir=p
                        virtual-store-dir=p].freeze,
          install_b: %w[D|dev no-optional P|prod].freeze,
          add: %w[allow-build config g|global save-catalog D|save-dev O|save-optional save-peer P|save-prod
                  save-catalog-name=b].freeze,
          update: %w[g|global i|interactive L|latest depth=i].freeze,
          dedupe: %w[check].freeze,
          run: %w[if-present no-bail parallel report-summary reporter-hide-prefix resume-from sequential].freeze,
          exec: %w[no-reporter-hide-prefix parallel report-summary resume-from c|shell-mode].freeze,
          pack: %w[json pack-destination=p pack-gzip-level=i out=p workspace-concurrency=i].freeze,
          rebuild: %w[filter=q].freeze,
          no: {
            install: %w[frozen-lockfile verify-store-integrity].freeze,
            add: %w[save-exact save-workspace-protocol].freeze
          }.freeze
        }.freeze
        OPT_YARN = {
          common: %w[check-files disable-pnp enable-pnp flat focus force frozen-lockfile json har ignore-engines
                     ignore-optional ignore-platform ignore-scripts link-duplicates no-bin-links no-default-rc
                     no-lockfile no-node-version-check no-progress non-interactive offline pnp prefer-offline prod
                     pure-lockfile s|silent skip-integrity-check strict-semver verbose cache-folder=p cwd=p emoji=b?
                     global-folder=p https-proxy=q link-folder=p modules-folder=p mutex=q network-concurrency=i
                     network-timeout=i preferred-cache-folder=p production=b? proxy=q otp=b registry=q update-checksums
                     use-yarnrc=p].freeze,
          install: %w[A|audit g|global S|save D|save-dev E|save-exact P|save-peer O|save-optional T|save-tilde].freeze,
          add: %w[A|audit D|dev E|exact O|optional P|peer T|tilde ignore-workspace-root-check].freeze,
          update: %w[A|audit C|caret E|exact L|latest T|tilde P|pattern=q S|scope=b].freeze,
          run: %w[scripts-prepend-node-path=b?].freeze
        }.freeze
        OPT_BERRY = {
          install: %w[check-cache check-resolutions immutable immutable-cache inline-builds json refresh-lockfile
                      mode=b].freeze,
          add: %w[cached D|dev json O|optional P|peer prefer-dev].freeze,
          add_a: %w[C|caret E|exact F|fixed interactive T|tilde mode=b].freeze,
          update: %w[R|recursive].freeze,
          dedupe: %w[check json mode=b strategy=b].freeze,
          run: %w[B|binaries-only inspect inspect-brk T|top-level require=q].freeze,
          pack: %w[n|dry-run install-if-needed json o|out=p].freeze
        }.freeze
        OPT_TSC = {
          base: %w[all b|build init listFilesOnly locale=b p|project=p showConfig w|watch].freeze,
          compiler: %w[allowArbitraryExtensions=!? allowImportingTsExtensions=!? allowJs=!?
                       allowSyntheticDefaultImports=!? allowUmdGlobalAccess=!? allowUnreachableCode=!?
                       allowUnusedLabels=!? alwaysStrict=!? assumeChangesOnlyAffectDirectDependencies=!? baseUrl=p
                       charset=b checkJs=!? composite=!? customConditions=q d|declaration=!? declarationDir=p
                       declarationMap=!? diagnostics=!? disableReferencedProjectLoad=!? disableSizeLimit=!?
                       disableSolutionSearching=!? downlevelIteration=!? emitBOM=!? emitDeclarationOnly=!?
                       emitDecoratorMetadata=!? erasableSyntaxOnly=!? esModuleInterop=!? exactOptionalPropertyTypes=!?
                       experimentalDecorators=!? explainFiles=!? extendedDiagnostics=!?
                       forceConsistentCasingInFileNames=!? generateCpuProfile=b importHelpers=!?
                       importsNotUsedAsValues=b incremental=!? inlineSourceMap=!? inlineSources=!?
                       isolatedDeclarations=!? isolatedModules=!? jsx=b jsxFactory=q jsxFragmentFactory=q
                       jsxImportSource=q keyofStringsOnly=!? lib=q libReplacement=!? listEmittedFiles=!? listFiles=!?
                       mapRoot=p maxNodeModuleJsDepth=i m|module=b moduleDetection=b moduleResolution=b moduleSuffixes=b
                       newLine=b noCheck=!? noEmit=!? noEmitHelpers=!? noEmitOnError=!? noErrorTruncation=!?
                       noFallthroughCasesInSwitch=!? noImplicitAny=!? noImplicitOverride=!? noImplicitReturns=!?
                       noImplicitThis=!? noImplicitUseStrict=!? noLib=!? noPropertyAccessFromIndexSignature=!?
                       noResolve=!? noStrictGenericChecks=!? noUncheckedIndexedAccess=!? noUncheckedSideEffectImports=!?
                       noUnusedLocals=!? noUnusedParameters=!? outDir=p outFile=p paths=q plugins=b
                       preserveConstEnums=!? preserveSymlinks=!? preserveValueImports=!? preserveWatchOutput=!?
                       pretty=!? reactNamespace=b removeComments=!? resolveJsonModule=!? resolvePackageJsonExports=!?
                       resolvePackageJsonImports=!? rewriteRelativeImportExtensions=!? rootDir=p rootDirs=p
                       skipDefaultLibCheck=!? skipLibCheck=!? sourceMap=!? sourceRoot=p stopBuildOnErrors=!? strict=!?
                       strictBindCallApply=!? strictBuiltinIteratorReturn=!? strictFunctionTypes=!? strictNullChecks=!?
                       strictPropertyInitialization=!? stripInternal=!? suppressExcessPropertyErrors=!?
                       suppressImplicitAnyIndexErrors=!? t|target=b traceResolution=!? tsBuildInfoFile=p typeRoots=p
                       types=b useDefineForClassFields=!? useUnknownInCatchVariables=!? verbatimModuleSyntax=!?].freeze,
          build: %w[clean=!? dry=!? force=!? v|verbose=!?].freeze,
          watch: %w[excludeDirectories=p excludeFiles=p fallbackPolling=b synchronousWatchDirectory=!? watchDirectory=b
                    watchFile=b].freeze
        }.freeze
        PASS_NODE = {
          tsc: %w[excludeDirectories excludeFiles customConditions lib moduleSuffixes plugins rootDirs typeRoots
                  types].freeze
        }.freeze
        private_constant :OPT_NPM, :OPT_PNPM, :OPT_YARN, :OPT_BERRY, :OPT_TSC, :PASS_NODE

        class << self
          def tasks
            %i[outdated update publish].freeze
          end

          def batchargs
            [ref, { refresh: %i[build copy] }].freeze
          end

          def aliasargs
            [ref, { refresh: :build }].freeze
          end

          def bannerargs
            %i[version dependfile].freeze
          end

          def prod?
            ENV['NODE_ENV'] == 'production'
          end

          def config?(val)
            return false unless (val = as_path(val))

            val.join('package.json').exist?
          end
        end

        subtasks({
          'package' => %i[install add update dedupe rebuild reinstall].freeze,
          'outdated' => %i[major minor patch].freeze,
          'bump' => %i[version major minor patch].freeze,
          'publish' => %i[latest tag].freeze,
          'tsc' => %i[project build].freeze,
          'add' => nil,
          'run' => nil,
          'exec' => nil,
          'nvm' => nil,
          'pack' => nil
        })

        def initialize(*, init: nil, ts: 'tsconfig.json', asdf: 'nodejs', **kwargs)
          super
          if @pass.include?(Node.ref)
            initialize_ref Node.ref
            initialize_logger(**kwargs)
          else
            initialize_build(Node.ref, prod: prod?, **kwargs)
            initialize_env(**kwargs)
          end
          @dependname = 'package.json'
          dependfile_set [@dependname]
          @tsfile = basepath! ts
          @pm = { __: init }
        end

        def ref
          Node.ref
        end

        def populate(*, **)
          super
          return unless (outdated? && ref?(Node.ref)) || @only

          namespace name do
            Node.subtasks do |action, flags|
              next if task_pass?(action)

              if flags.nil?
                case action
                when 'add'
                  format_desc action, nil, 'save?=[=-]prod|dev|optional|peer|bundle,(-)name+'
                  task action, [:save] do |_, args|
                    packages = if args.save =~ /\A([=-]*)?(prod|dev|optional|peer|bundle)\z/
                                 save = [$2, $1.include?('='), $1.include?('-')]
                                 args.extras
                               else
                                 save = 'prod'
                                 args.to_a
                               end
                    param_guard(action, 'name', args: packages)
                    depend(:add, packages: packages, save: save)
                  end
                when 'run'
                  next if scripts.empty?

                  format_desc action, nil, "script,opts*|#{indexchar}index+|#,pattern*"
                  task action, [:script] do |_, args|
                    list = scripts.to_a
                    if args.script == '#'
                      format_list(list, "run[#{indexchar}N]", 'scripts', grep: args.extras, from: dependfile)
                    else
                      args = param_guard(action, 'script', args: args.to_a)
                      opts = []
                      args.each do |val|
                        if (n, extra = indexitem(val))
                          if (item = list[n.pred])
                            run compose([item.first, extra].compact.join(' '), script: true)
                          elsif exception
                            indexerror n, list
                          else
                            next log.warn "run script #{n} of #{list.size} (out of range)"
                          end
                        else
                          opts << val
                        end
                      end
                      next if opts.empty?

                      list = if (yarn = dependtype(:yarn)) > 0
                               yarn == 1 ? OPT_YARN[:run] + OPT_YARN[:common] : OPT_BERRY[:run]
                             elsif pnpm?
                               pnpmopts :run, :common_filter
                             else
                               npmopts :run
                             end
                      OptionPartition.new(opts, list, session(dependbin, 'run'), project: self)
                                     .add_first
                                     .append(delim: true, quote: false)
                      run(from: :run)
                    end
                  end
                when 'exec'
                  format_desc action, nil, 'pkg/cmd,opts*,args*'
                  task action, [:package] do |_, args|
                    if (package = args.package)
                      args = args.extras
                      cmd = if pnpm?
                              pre = ->(ch) { "-#{ch}" if args.delete(ch) }
                              list = pnpmopts :exec, :common_filter
                              session 'pnpm', pre.call('r'), pre.call('c'), 'exec'
                            else
                              list = npmopts :exec
                              session 'npm', 'exec'
                            end
                      op = OptionPartition.new(args, list, cmd, project: self)
                      if op.empty?
                        op << package
                        if (args = readline('Enter arguments', force: false))
                          op.delim unless pnpm?
                          op << args
                        end
                      else
                        op.delim unless pnpm?
                        op << package << op.join(' ')
                      end
                    else
                      session 'npm', 'exec', quote_option('c', readline('Enter command', force: true), double: true)
                    end
                    run(from: :exec)
                  end
                when 'nvm'
                  next unless ENV['NVM_DIR']

                  format_desc action, nil, 'version,args*'
                  task action, [:version] do |_, args|
                    version = param_guard(action, 'version', args: args, key: :version)
                    args = args.extras
                    args << readline('Enter command', force: true) if args.empty?
                    args.unshift(File.join(ENV['NVM_DIR'], 'nvm-exec'))
                    run(args.join(' '), { 'NODE_VERSION' => version }, banner: false, from: :nvm)
                  end
                when 'pack'
                  format_desc action, nil, 'opts*'
                  task action do |_, args|
                    pack args.to_a
                  end
                end
              else
                namespace action do
                  flags.each do |flag|
                    case action
                    when 'outdated'
                      format_desc action, flag, "#{shortname('i', 's', 'u', 'd')},diff"
                      task flag do |_, args|
                        outdated flag, args.to_a
                      end
                    when 'package'
                      format_desc(action, flag, 'opts*', before: case flag
                                                                 when :dedupe, :rebuild then nil
                                                                 when :reinstall then 'force?'
                                                                 else 'name*'
                                                                 end)
                      task flag do |_, args|
                        package flag, args.to_a
                      end
                    when 'bump'
                      break unless version

                      if flag == :version
                        format_desc action, flag, 'version'
                        task flag, [:version] do |_, args|
                          version = param_guard(action, flag, args: args, key: :version)
                          bump flag, version
                        end
                      else
                        format_desc action, flag
                        task flag do
                          bump flag
                        end
                      end
                    when 'publish'
                      format_desc(action, flag, 'otp?,p/ublic|r/estricted?,d/ry-run?', before: ('tag' if flag == :tag))
                      task flag do |_, args|
                        args = args.to_a
                        access = if has_value!(args, 'r', 'restricted')
                                   'restricted'
                                 elsif has_value!(args, 'p', 'public')
                                   'public'
                                 end
                        dryrun = has_value!(args, 'd', 'dry-run')
                        if flag == :latest
                          otp = args.first
                        else
                          tag, otp = param_guard(action, flag, args: args)
                        end
                        publish(flag, otp: otp, tag: tag, access: access, dryrun: dryrun)
                      end
                    when 'tsc'
                      break unless @tsfile

                      format_desc(action, flag, 'opts*', "#{flag == :project ? 'before' : 'after'}": 'config?')
                      task flag do |_, args|
                        args = args.to_a
                        if flag == :project
                          project = if exist?(args.first)
                                      args.shift
                                    else
                                      @tsfile
                                    end
                        end
                        watch = has_value!(args, 'w', 'watch')
                        tsc(*args, banner: true, project: project, build: flag == :build, watch: !watch.nil?)
                      end
                    end
                  end
                end
              end
            end
          end
        end

        def copy(from: 'build', into: 'node_modules', scope: nil, also: nil, create: nil, files: nil, workspace: false,
                 link: false, force: false, override: false, sync: invoked_sync?('copy'), **kwargs)
          return if @copy == false

          glob = kwargs[:include]
          pass = kwargs[:exclude]
          if @copy && !override
            return super unless @copy.is_a?(Hash)

            from = @copy[:from] if @copy.key?(:from)
            into = @copy[:into] if @copy.key?(:into)
            files = @copy[:files] if @copy.key?(:files)
            workspace = @copy[:workspace] if @copy.key?(:workspace)
            link = @copy[:link] if @copy.key?(:link)
            force = @copy[:force] if @copy.key?(:force)
            scope = @copy[:scope] if @copy.key?(:scope)
            also = @copy[:also] if @copy.key?(:also)
            create = @copy[:create] if @copy.key?(:create)
            glob = @copy[:include] if @copy.key?(:include)
            pass = @copy[:exclude] if @copy.key?(:exclude)
          end
          items = []
          if build? && path != @workspace.home && @workspace.home?
            items << @workspace.home
            @workspace.rev_clear(@workspace.find(@workspace.home).name, sync: sync)
          end
          items.concat(as_a(also)) if also
          return if items.empty?

          on :first, :copy
          print_item unless @output[0] || silent? || task_invoked?(/^copy(?::#{Node.ref}|$)/)
          packed = false
          items.each do |dir|
            case dir
            when Pathname
              dest = dir
              @workspace.rev_clear(dest, sync: sync)
            when String
              dest = @workspace.rootpath(dir)
              @workspace.rev_clear(dest, sync: sync)
            when Symbol
              dest = if (proj = @workspace.find(name: dir))
                       @workspace.rev_clear(proj.name, sync: sync)
                       proj.path
                     else
                       log.warn message("copy project :#{dir}", hint: 'missing')
                       nil
                     end
            when Hash
              from = dir[:from] if dir.key?(:from)
              into = dir[:into] if dir.key?(:into)
              scope = dir[:scope] if dir.key?(:scope)
              link = dir[:link] if dir.key?(:link)
              force = dir[:force] if dir.key?(:force)
              dest = dir[:target]
              create = dir[:create]
              workspace = dir[:workspace]
              glob = dir[:include]
              pass = dir[:exclude]
              dest = items.first unless dest && dest != true
              @workspace.rev_clear(dest, sync: sync) unless dest == true
            when Project::Base
              dest = dir.path
              @workspace.rev_clear(dir.name, sync: sync)
            else
              raise_error TypeError, "unknown: #{dir}", hint: 'copy'
            end
            next unless from && dest&.directory?

            if from == :npm
              begin
                unless packed
                  require 'open3'
                  files = pwd_set do
                    Open3.capture2e(session_output('npm', 'pack --dry-run --no-color', npmname).to_s)
                         .first
                         .scan(/^npm notice \d+(?:\.\d+)?[a-z]+ (.+)$/i)
                         .map! { |item| Pathname.new(item.first) }
                         .select(&:exist?)
                  end.concat(Array(files))
                  packed = true
                end
                base = dest.join(into, npmname)
                base.mkpath
                log.info "cp npm:#{npmname} #{base}"
                subdir = []
                errors = 0
                files.each do |file|
                  s, d = file.is_a?(Array) ? file : [file, file]
                  dest = base + d
                  unless subdir.include?((target = dest.dirname).to_s)
                    target.mkpath
                    subdir << target.to_s
                  end
                  FileUtils.cp(basepath(s), dest, verbose: !silent?)
                rescue StandardError => e
                  print_error e
                  errors += 1
                end
              rescue StandardError => e
                on_error e, :copy
              else
                puts message(base, subdir.size, files.size - errors) unless silent?
              end
              next
            end
            glob = Array(glob || '**/*')
            target = []
            from = basepath from
            if workspace
              from.glob('*').each do |entry|
                next unless entry.directory?

                sub = if (proj = @workspace.find(entry))
                        proj.packagename
                      elsif (file = entry + dependname).exist?
                        begin
                          doc = JSON.parse(file.read)
                          doc['name']
                        rescue StandardError => e
                          log.error e
                          raise if exception
                        end
                      end
                if sub
                  target << [entry, dest.join(into, sub)]
                else
                  log.debug message("#{dependname} in \"#{entry}\"", hint: 'missing')
                end
              end
            else
              target << [from, dest.join(into, scope || npmname)]
            end
            target.each do |src, to|
              glob.each { |val| log.info "cp #{from + val} #{to}" }
              copy_dir(src, to, glob, create: create, link: link, force: force, pass: pass, verbose: !silent?)
            rescue StandardError => e
              on_error e, :copy
            end
          end
          on :last, :copy
        end

        def depend(flag = nil, *, sync: invoked_sync?('depend', flag), packages: [], save: nil, exact: nil,
                   omit: env('NPM_OMIT'), **)
          if @depend && !flag
            super
          elsif outdated?
            workspace.rev_clear(name, sync: sync)
            return update if !flag && env('NODE_UPDATE')

            add = flag == :add
            if add
              remove, packages = packages.partition { |val| val.delete_prefix!('-') }
              remove.quote!
            end
            save, exact, omit = save if save.is_a?(Array)
            ws = env('NODE_WORKSPACES', equals: '0')
            om = lambda do |cmd|
              if omit
                save = case save
                       when 'peer'
                         'optional'
                       when 'optional'
                         'dev'
                       when 'dev'
                         'prod'
                       end
              end
              return unless save && save != 'bundle'

              cmd << "--#{save}"
            end
            rm = lambda do |target|
              return if remove.empty?

              run(target.temp(*remove).sub!(/ (?:add|install) /, ' remove '), from: :remove, sync: sync)
            end
            if (yarn = dependtype(:yarn)) > 0
              cmd = session('yarn', flag || 'install')
              append_loglevel
              if yarn == 1
                cmd << '--ignore-engines' unless option('ignore-engines', equals: '0')
                cmd << '--ignore-scripts' if option('ignore-scripts')
                cmd << '--force' if option('force')
              else
                cmd << '--mode=skip-build' if option('ignore-scripts')
                cmd << '--check-cache' if !flag && option('force')
              end
              if nolockfile?('yarn')
                cmd << '--no-lockfile'
              elsif option('ci')
                if yarn == 1
                  cmd << '--frozen-lockfile'
                elsif !flag
                  cmd << '--immutable' << '--refresh-lockfile'
                end
              end
              if add
                cmd << '-W' if yarn == 1 && !option('w', 'ignore-workspace-root-check', equals: '0')
                rm.call(cmd)
                om.call(cmd)
                cmd << '--exact' if exact
              end
            elsif pnpm?
              cmd = session('pnpm', flag || 'install')
              append_nocolor
              append_loglevel
              if add
                om.call(cmd)
                rm.call(cmd)
                cmd << '--save-exact' if exact
                option('allow-build') { |val| cmd << quote_option('allow-build', val) }
              else
                append_platform
              end
              option('public-hoist-pattern') do |val|
                split_escape(val) { |opt| cmd << shell_option('public-hoist-pattern', opt) }
              end
              cmd << '--ignore-workspace' if ws
              cmd << if option('force')
                       '--force'
                     elsif nolockfile?('pnpm')
                       '--no-lockfile'
                     elsif option('ci')
                       '--frozen-lockfile'
                     end
              cmd << '--ignore-scripts' if option('ignore-scripts')
              cmd << '--dangerously-allow-all-builds' if option('approve-builds')
            else
              cmd = session 'npm'
              cmd << (ci = option('ci') ? 'ci' : 'install')
              cmd << '--workspaces=false' if ws
              append_nocolor
              append_loglevel
              if omit
                cmd << "--omit=#{save || omit}"
                save = nil
              end
              unless ci
                if add
                  cmd << "--save-#{save}" if save
                  rm.call(cmd)
                  cmd << '--save-exact' if exact
                else
                  append_platform
                end
              end
              cmd << '--package-lock=false' << 'save=false' if nolockfile?('npm')
              cmd << '--ignore-scripts' if option('ignore-scripts')
            end
            if add
              return if packages.empty?

              cmd.merge(packages.quote!)
            end
            run(from: flag || :depend, sync: sync)
          end
        end

        def outdated(flag = nil, opts = [], sync: invoked_sync?('outdated', flag))
          cmd = session(pnpm? ? 'pnpm' : 'npm', 'outdated')
          dryrun = has_value?(opts, 'd', 'dry-run') || dryrun?
          unless dryrun
            log.info cmd.to_s
            on :first, :outdated
          end
          banner = format_banner(cmd.temp(('--dry-run' if dryrun)))
          print_item banner if sync
          begin
            data = pwd_set(dryrun: dryrun) { `#{cmd.temp('--json --loglevel=error')}` }
            doc = dependfile.read
            json = JSON.parse(doc)
            dep1 = json['dependencies'] || {}
            dep2 = json['devDependencies'] || {}
            target = json['name']
          rescue StandardError => e
            on_error(e, :outdated, dryrun: dryrun)
            return
          end
          found = []
          avail = []
          flag ||= case (up = option('u', 'update'))
                   when 'major', 'minor'
                     up.to_sym
                   else
                     prod? ? :patch : :minor
                   end
          if sync && !stdin?
            items = if has_value?(opts, 's', 'select')
                      se = true
                      []
                    elsif has_value?(opts, 'i', 'interactive')
                      ia = true
                      []
                    end
          end
          unless data.empty?
            JSON.parse(data).each_pair do |key, val|
              val = val.find { |obj| obj['dependent'] == target } if val.is_a?(Array)
              next unless val && (file = dep1[key] || dep2[key]) && file != '*'

              latest = val['latest']
              ch = file[0]
              if ch.match?(/[~^]/)
                file = file[1..-1]
              elsif ia && flag == :major
                major = true
              else
                avail << [key, file, latest, true]
                next
              end
              current = val['current'] || file
              want = val['wanted']
              unless latest[SEM_VER, 6]
                case flag
                when :major
                  want = latest
                when :minor
                  want = latest if latest[SEM_VER, 1] == want[SEM_VER, 1]
                when :patch
                  if (g = latest.match(SEM_VER)) && (h = want.match(SEM_VER)) && g[1] == h[1] && g[3] == h[3]
                    want = latest
                  end
                end
              end
              next unless (current != want || file != want) && (want.match?(SEM_VER) || !file.match?(SEM_VER))

              f = semscan file
              w = semscan want
              a = f[0]
              b = f[2]
              c = w[0]
              d = w[2]
              upgrade = case flag
                        when :major
                          a == '0' ? c == '0' || c == '1' : true
                        when :minor
                          ch == '^' && (a == '0' ? c == '0' && b == d : a == c)
                        when :patch
                          a == c && b == d && f[4] != w[4]
                        end
              if upgrade && !w[5]
                next if file == want

                found << [key, file, want, if a != c
                                             1
                                           elsif b != d
                                             a == '0' ? 1 : 3
                                           else
                                             5
                                           end, major, f, w]
              elsif !major
                avail << [key, file, latest, latest != current]
              end
            end
          end
          pending = 0
          modified = 0
          width = ->(a, i) { a.map { |aa| aa[i] }.max_by(&:size).size }
          pad = ->(val, ord) { val.succ.to_s.rjust([ord.size.to_s.size, 2].max) }
          footer = lambda do |val, size|
            return unless verbose

            msg, hint = if modified == -1
                          ['Packages were updated', 'more possible']
                        else
                          ['No packages were updated', 'possible']
                        end
            possible = pending + val
            puts print_footer(empty_status(msg, hint, possible == size ? 0 : possible))
          end
          print_item banner unless sync
          if !found.empty?
            col1 = width.call(found, 0) + 4
            col2 = width.call(found, 1) + 4
            col3 = pad.call(found.size, found).size + 2 + col1 + col2 + width.call(found, 2)
            packages = []
            pat = ->(a) { /("#{Regexp.escape(a[0])}"\s*:\s*)"([~^])#{'?' if a[4]}#{Regexp.escape(a[1])}"/ }
            edit = lambda do |a, pkg, mod|
              packages << a[0]
              modified += 1
              "#{pkg}\"#{mod || (a[3] == 1 && a[4] ? '^' : '')}#{a[2]}\""
            end
            found.each_with_index do |item, i|
              a, b, c, d, e = item
              cur = modified
              doc.send(items ? :sub : :sub!, pat.call(item)) do |capture|
                if $2 == '~' && flag != :patch
                  cur = -1
                  pending += 1
                  capture
                else
                  edit.call(item, $1, $2)
                end
              end
              a = a.ljust(col1)
              b = b.ljust(col2)
              sub_style! b, theme[:current] if theme[:current] && !stdin?
              if cur == -1
                c = 'SKIP'
              elsif modified == cur
                c = 'FAIL'
              elsif !stdin?
                if d == 1
                  sub_style! a, theme[:major]
                  sub_style! c, :bold, color(:green)
                else
                  sub_style!(c, **opt_style(color(d == 3 ? :green : :yellow), SEM_VER, d))
                end
                g = item
              end
              s = "#{pad.call(i, found)}. #{a}#{b}#{c}"
              if se
                items << [s, g]
                next
              elsif ia && g
                items << [g]
                if flag != :major || e || semmajor?(item[5], item[6])
                  items.pop unless confirm_semver(s.ljust(col3 + s.size - s.stripstyle.size), (d / 2.0).ceil)
                  next
                end
              end
              puts s
            end
            pending = avail.reduce(pending) { |a, b| a + (b[3] ? 0 : 1) }
            if (dryrun && Array(items).empty?) || (modified == 0 && (pending > 0 || (items && pending == 0)))
              n = if items
                    if items.empty?
                      puts 'No updates were selected'
                      0
                    else
                      puts items.map(&:first) if se
                      items.size
                    end
                  else
                    found.size
                  end
              footer.call(modified, n) unless n == 0
            elsif modified > 0
              if items
                packages.clear
                if ia
                  (1..items.size)
                else
                  choice('Select a package', items.map(&:first), multiple: true, force: false, index: true,
                                                                 border: true)
                end.each do |n|
                  item = items[n.pred].last
                  doc.sub!(pat.call(item)) { edit.call(item, $1, $2) }
                end
              end
              unless packages.empty?
                modified = -1
                if dryrun
                  footer.call(0, found.size)
                else
                  File.write(dependfile, doc)
                  if sync && (opts.include?('diff') || option('diff'))
                    run(git_output('diff', shell_quote(dependfile)), banner: false)
                  end
                  if has_value?(opts, 'u', 'update') || up
                    package(:update, packages: packages, from: :'outdated:update')
                  else
                    footer.call(0, found.size)
                  end
                  printsucc
                  commit(:add, [dependname], pass: true)
                end
              end
            end
          elsif !avail.empty?
            col1 = width.call(avail, 0) + 4
            col2 = width.call(avail, 1)
            col3 = width.call(avail, 2) + 4
            avail.each_with_index do |item, i|
              a, b, c, d = item
              a = a.ljust(col1)
              b = sub_style b.ljust(col2), color(d ? :red : :yellow)
              c = c.ljust(col3)
              unless d
                sub_style! a, theme[:active]
                sub_style! c, color(:green)
                pending += 1
              end
              puts "#{pad.call(i, avail)}. #{(a + c + b).subhint(d ? 'locked' : 'latest')}"
            end
            footer.call(0, avail.size)
          else
            puts 'No updates were found'
          end
          on :last, :outdated unless dryrun
        end

        def update(*)
          package(:update, from: :update)
        end

        def publish(flag = nil, *, sync: invoked_sync?('publish', flag), otp: nil, tag: nil, access: nil, dryrun: nil)
          if read_package('private')
            ws = children.select { |proj| proj.ref?(Node.ref) }
            if ws.empty?
              print_error('nothing to publish', subject: name, hint: 'private')
            elsif confirm_basic('Publish workspace?', ws.map(&:name).join(', '), 'N')
              ws.each { |proj| proj.publish(flag, sync: sync, otp: otp, tag: tag, access: access, dryrun: dryrun) }
            end
            return
          end
          return print_error('version not found', subject: name, hint: dependname) unless version

          cmd = session 'npm', 'publish'
          cmd << basic_option('otp', otp) if otp ||= option('otp')
          cmd << basic_option('tag', tag.tr(' ', '-')) if tag ||= option('tag')
          case access || option('access')
          when 'p', 'public'
            cmd << '--access=public'
          when 'r', 'restricted'
            cmd << '--access=restricted'
          end
          dryrun ||= dryrun?('npm')
          if dryrun
            cmd << '--dry-run'
          else
            from = :'npm:publish'
            log.info cmd.to_s
          end
          if sync
            run(sync: sync, from: from, interactive: !dryrun && ['Publish', 'N', npmname])
          else
            require 'open3'
            on :first, from
            pwd_set(from: from, dryrun: dryrun) do
              Open3.popen2e(cmd = session_done(cmd)) do |_, out|
                write_lines(out, banner: format_banner(cmd),
                                 sub: npmnotice(opt_style(color(:bright_blue), /^(.+)(Tarball .+)$/, 2)))
              end
            end
            on :last, from
          end
        end

        def package(flag, opts = [], packages: [], from: nil)
          workspace.rev_clear(name)
          yarn = dependtype(:yarn)
          if yarn > 0 && !(yarn == 1 && ((flag == :update && !packages.empty?) || flag == :rebuild))
            cmd = session 'yarn', case flag
                                  when :update
                                    if yarn == 1
                                      'upgrade'
                                    else
                                      spec = 0
                                      'up'
                                    end
                                  when :reinstall
                                    if yarn == 1
                                      remove_modules 'yarn' if opts.include?('force')
                                    elsif opts.delete('force')
                                      opts << 'check-cache'
                                    end
                                    opts << 'no-lockfile' if lockfile(true)
                                    'install'
                                  when :add
                                    spec = 1
                                    'add'
                                  else
                                    yarn == 1 && flag == :dedupe ? 'install' : flag
                                  end
            op = OptionPartition.new(opts, if yarn == 1
                                             OPT_YARN.fetch(flag == :dedupe ? :install : flag, []) + OPT_YARN[:common]
                                           else
                                             OPT_BERRY.fetch(flag, []) + case flag
                                                                         when :add, :update then OPT_BERRY[:add_a]
                                                                         else []
                                                                         end
                                           end, cmd, project: self)
            if yarn == 1 && flag != :reinstall
              op << '--no-lockfile' if nolockfile?('yarn')
              op << '--ignore-engines' unless option('ignore-engines', equals: '0')
            end
          else
            args = if pnpm?
                     case flag
                     when :install, :update
                       opts << 'no-lockfile' if nolockfile?('pnpm')
                       spec = 0 if flag == :update
                     when :add
                       spec = 1
                     when :reinstall
                       opts << 'force'
                       flag = :install
                     end
                     flags = [flag]
                     unless flag == :rebuild
                       flags << :install_a
                       no = OPT_PNPM[:no][flag]
                     end
                     [
                       opts,
                       flags.yield_self do |out|
                         unless flag == :dedupe
                           out << :common_filter
                           unless flag == :add
                             out << :install_b
                             out << :common_cpu unless flag == :update
                           end
                         end
                         pnpmopts(*out)
                       end,
                       session('pnpm', flag)
                     ]
                   else
                     case flag
                     when :install, :update
                       opts.unshift('package-lock=false', 'save=false') if nolockfile?('npm')
                       spec = flag == :install ? 0 : 2
                     when :add
                       spec = 1
                       flag = :install
                     when :reinstall
                       remove_modules 'npm' if opts.delete('force')
                       opts.unshift('package-lock=false') if lockfile(true)
                       flag = :install
                     end
                     flags = [flag]
                     unless flag == :rebuild
                       flags << :install_a
                       unless flag == :dedupe
                         %w[save ignore-scripts strict-peer-deps].each do |key|
                           option(key, prefix: 'npm', ignore: false) do |val|
                             opts << case val
                                     when '0', 'false'
                                       "#{key}=false"
                                     else
                                       "#{key}=true"
                                     end
                           end
                         end
                         flags << :install_b
                       end
                       no = OPT_NPM[:no][:install]
                     end
                     [
                       opts,
                       npmopts(*flags),
                       session('npm', flag)
                     ]
                   end
            op = OptionPartition.new(*args, no: no, project: self)
            append_platform if flag == :install
            append_nocolor
          end
          append_loglevel
          case spec
          when Numeric
            op.each do |opt|
              if opt =~ op.values
                case $1
                when 'w', 'workspace'
                  op << quotepath($1, $2)
                end
              elsif opt.match?(/^-|=/)
                op.errors << opt
              else
                op.found << (spec == 2 && (n = opt.index('@')) ? opt[0, n] : opt)
              end
            end
            op.swap
              .concat(packages)
            raise_error ArgumentError, 'no packages to add' if op.empty? && spec == 1
            op.append(quote: true)
              .clear(errors: true)
          else
            op.clear
          end
          run(from: from || :"package:#{flag}")
        end

        def bump(flag, val = nil)
          return unless val ||= sembump(version, flag)

          doc = dependfile.read
          if doc.sub!(/"version"\s*:\s*"#{version}"/, "\"version\": \"#{val}\"")
            unless dryrun?
              log.info "bump version #{version} to #{val.subhint(flag)}"
              on :first, :bump
              dependfile.write(doc)
            end
            if stdin?
              puts val
            elsif !silent?
              major = flag == :major
              emphasize("version: #{val}", title: name, border: borderstyle, sub: [
                headerstyle,
                opt_style(color(major ? :green : :yellow), /\A(version:)( )(\S+)(.*)\z/, 3),
                opt_style(theme[major ? :major : :active], /\A(version:)(.*)\z/)
              ])
            end
            unless dryrun?
              commit(:add, [dependname], pass: true)
              on :last, :bump
            end
          else
            raise_error 'version not found', hint: dependfile
          end
        rescue StandardError => e
          on_error(e, :bump, dryrun: dryrun?)
        end

        def pack(opts = [])
          return unless version

          cmd = session dependbin, 'pack'
          if dependtype(:yarn) > 1
            op = OptionPartition.new(opts, OPT_BERRY[:pack], cmd, project: self)
            op.append?('out', Pathname.pwd + "#{project}-#{version}.tgz")
          else
            op = OptionPartition.new(opts, pnpm? ? OPT_PNPM[:pack] : npmopts(:pack), cmd, project: self)
            unless pnpm?
              op.each do |opt|
                next unless opt =~ op.values

                case $1
                when 'w', 'workspace'
                  op << quotepath($1, $2)
                  op.found << opt
                end
              end
            end
            op.append?('pack-destination', Dir.pwd)
          end
          op.clear
          run(from: :pack)
        end

        def tsc(*args, with: nil, pass: PASS_NODE[:tsc], sync: true, banner: verbose?, from: :tsc, **kwargs)
          session_apply(with, args: args, kwargs: kwargs, pass: pass) if with
          p = kwargs[:project]
          b = kwargs[:build]
          w = kwargs[:watch]
          list = OPT_TSC[:base] + OPT_TSC[:compiler]
          cmd = session 'tsc', if p
                                 quote_option 'p', basepath(p)
                               elsif b
                                 list.concat(OPT_TSC[:build])
                                 '-b'
                               end
          if w
            list.concat(OPT_TSC[:watch])
            cmd << '-w'
          end
          op = OptionPartition.new(args, list, cmd, project: self, sep: ' ')
          unless p
            if b.is_a?(String)
              op.add_path(b)
            elsif w.is_a?(String)
              op.add_path(w)
            else
              op.exist?(add: true)
            end
          end
          op.clear
          cmd = session_done(op.target)
          print_run(cmd, banner, **kwargs)
          session 'npx', cmd
          start = time_epoch if kwargs.fetch(:verbose, verbose? && !stdin?)
          run(sync: sync, banner: banner, exception: kwargs.fetch(:exception, exception), from: from).tap do |ret|
            next unless success?(ret, banner, start.nil?) && start

            print_status(name, subject: 'tsc', start: start, from: :completed)
          end
        end

        def compose(target, opts = nil, script: false, args: nil, from: nil, **)
          return unless target

          if script
            ret = session dependbin, 'run'
            raise_error "#{dependbin} run: #{target}", hint: from unless append_any(target, build: true)
            append_any opts if opts
            append_loglevel
            append_any(args, delim: true) if args
            ret
          else
            case target
            when String
              target
            when Hash
              append_hash(target, target: []).join(' ')
            when Enumerable
              target.to_a.join(' ')
            else
              raise_error TypeError, "unknown: #{target}", hint: 'compose'
            end
          end
        end

        def depend?
          @depend != false && (!@depend.nil? || outdated?)
        end

        def outdated?
          dependfile.exist? && !task_pass?('outdated')
        end

        def update?
          outdated?
        end

        def refresh?
          !Node.prod?
        end

        def yarn?
          (@pm[:yarn] ||= if rootpath('yarn.lock', ascend: dependroot).exist?
                            yarntype
                          elsif (ver = read_package || read_install)
                            if ver =~ /^yarn(?:@(\d)|$)/
                              $1 && $1.to_i > 1 ? yarntype : 1
                            else
                              0
                            end
                          else
                            case @pm[:__]
                            when 'yarn'
                              1
                            when 'berry'
                              yarntype
                            else
                              0
                            end
                          end) > 0
        end

        def pnpm?
          (@pm[:pnpm] ||= if rootpath('pnpm-lock.yaml', ascend: dependroot).exist?
                            pnpmtype
                          elsif (ver = read_package || read_install)
                            ver.start_with?('pnpm') ? pnpmtype : 0
                          else
                            @pm[:__] == 'pnpm' ? pnpmtype : 0
                          end) > 0
        end

        def workspaces?
          if pnpm?
            exist?('pnpm-workspace.yaml')
          else
            read_package('workspaces').is_a?(Array)
          end
        end

        def dev?
          super && (!Node.prod? || (@dev == true && !prod?))
        end

        def prod?
          @prod != false && (Node.prod? || super)
        end

        def dependtype(prog)
          return @pm[prog] if @pm.key?(prog)

          meth = :"#{prog}?"
          respond_to?(meth) && __send__(meth) ? @pm[prog] : 0
        end

        def dependbin
          if yarn?
            'yarn'
          else
            pnpm? ? 'pnpm' : 'npm'
          end
        end

        def version
          @version ||= read_package('version')
        end

        def packagename
          read_package 'name'
        end

        def scripts
          @scripts ||= read_package('scripts').yield_self { |ret| ret.is_a?(Hash) ? ret : {} }
        end

        private

        def read_package(key = 'packageManager', update: false)
          if !@pm.key?(key) || update
            doc = JSON.parse(dependfile.read)
            @pm[key] = case key
                       when 'packageManager'
                         if (val = doc['packageManager'])
                           (n = val.index('+')) ? val[0, n] : val
                         else
                           false
                         end
                       else
                         doc[key]
                       end
            unless @pm[:_]
              %w[name scripts version private workspaces].each { |s| @pm[s] = doc[s] }
              @pm[:_] = true
            end
          end
        rescue StandardError => e
          log.debug e
          @pm[key] = nil
        else
          @pm[key]
        end

        def read_install
          env('NODE_INSTALL') do |ret|
            if ret.include?(',')
              catch :found do
                split_escape(ret) do |val|
                  case val
                  when /^yarn/
                    next if yarntype(exist: true) == 0
                  when /^pnpm/
                    next if pnpmtype(exist: true) == 0
                  when /^npm/
                    nil
                  else
                    next
                  end
                  ret = val
                  throw :found
                end
                return
              end
            end
            @pm['packageManager'] ||= ret
            ret
          end
        end

        def yarntype(exist: false)
          if (rc = rootpath('.yarnrc.yml', ascend: dependroot)).exist?
            require 'yaml'
            doc = YAML.load_file(rc)
            doc.nodeLinker == 'node-modules' ? 2 : 3
          elsif exist && !exist?('yarn.lock')
            0
          else
            1
          end
        rescue StandardError => e
          return 0 if exist

          log.debug e
          3
        end

        def pnpmtype(exist: false)
          require 'yaml'
          doc = YAML.load_file(basepath('node_modules/.modules.yaml', ascend: dependroot))
          @pm['packageManager'] = doc['packageManager']
          case doc['nodeLinker']
          when 'hoisted'
            1
          when 'pnp'
            3
          else
            4
          end
        rescue StandardError => e
          if exist
            %w[pnpm-lock.yaml pnpm-workspace.yaml].any? { |val| exist?(val) } ? 4 : 0
          else
            log.debug e
            4
          end
        end

        def remove_modules(prefix = dependbin)
          modules = basepath 'node_modules'
          return false unless modules.directory? && confirm_basic('Remove?', modules, prefix: prefix)

          modules.rmtree
        rescue Timeout::Error => e
          puts
          print_error(e, hint: modules, pass: true)
          exit 1
        rescue StandardError => e
          print_error(e, pass: true)
          false
        else
          true
        end

        def append_loglevel(target: @session)
          level = env('NODE_LOGLEVEL')
          silent = silent? || level == 'silent'
          return unless silent || level

          if yarn?
            if dependtype(:yarn) == 1
              if silent
                target << '--silent'
              elsif level == 'verbose'
                target << '--verbose'
              end
            end
          elsif pnpm?
            if silent
              target << '--reporter=silent'
              level ||= 'error'
            end
            case level
            when 'debug', 'info', 'warn', 'error'
              target << basic_option('loglevel', level)
            end
          elsif silent
            target << '--loglevel=silent'
          else
            case level
            when 'error', 'warn', 'notice', 'http', 'info', 'verbose', 'silly'
              target << basic_option('loglevel', level)
            end
          end
        end

        def append_platform(target: @session)
          %w[cpu os libc].each do |name|
            next unless (val = option(name))

            target << basic_option(name, val)
          end
        end

        def quotepath(name, val)
          if $2.include?(File::SEPARATOR) || (workspace.windows? && val.match?(%r{[\\/]}))
            quote_option name, basepath(val)
          else
            shell_option name, val
          end
        end

        def dependroot
          dependname if parent&.has?('outdated', Node.ref)
        end

        def npmname
          packagename || project
        end

        def npmnotice(*args)
          [
            opt_style(color(:bright_cyan), /^(npm error )(code|\d+)(.+)$/, 2),
            opt_style(color(:bright_red), /^(npm )(error)(.*)$/, 2),
            opt_style(color(:bright_cyan), /^(npm )(notice)(.*)$/, 2),
            opt_style(:bold, /^(npm )(.+)$/)
          ].concat(args)
        end

        def npmopts(*args)
          OPT_NPM[:common] + args.flat_map { |name| OPT_NPM.fetch(name, []) }
        end

        def pnpmopts(*args)
          OPT_PNPM[:common] + args.flat_map { |name| OPT_PNPM.fetch(name, []) }
        end

        def lockfile(delete = false)
          file = basepath(if yarn?
                            'yarn.lock'
                          else
                            pnpm? ? 'pnpm-lock.yaml' : 'package-lock.json'
                          end)
          if file.exist?
            if delete
              file.delete
              return
            end
            file
          elsif (file = rootpath(file.basename, ascend: dependroot)).exist?
            file
          end
        rescue StandardError => e
          log.debug e
          file
        end

        def nolockfile?(prefix = dependbin)
          option('package-lock', 'lockfile', prefix: prefix, equals: '0') || !option('no-lockfile', prefix: prefix).nil?
        end

        def dryrun?(prefix = dependbin)
          super(target: @session, prefix: prefix)
        end
      end

      Application.implement Node

      class Python < Git
        DEP_PYTHON = %w[poetry.lock setup.cfg pyproject.toml setup.py requirements.txt].freeze
        DIR_PYTHON = (DEP_PYTHON + %w[README.rst]).freeze
        OPT_PYTHON = {
          common: %w[b=+ B d E h i I O P q=+ s S u v=+ V=+ x c=q m=b W=b X=q check-hash-based-pycs=b].freeze,
          build: %w[C=bm n|no-isolation s|sdist x|skip-dependency-check v|verbose w|wheel config-json=q config-setting=q
                    installer=b o|outdir=p].freeze,
          venv: %w[clear copies symlinks system-site-packages upgrade upgrade-deps without-scm-ignore-files without-pip
                   prompt=q].freeze
        }.freeze
        OPT_PIP = {
          common: %w[debug disable-pip-version-check isolated no-cache-dir no-color no-input require-virtualenv
                     q|quiet=+ v|verbose=+ cache-dir=p cert=p client-cert=p exists-action=b keyring-provider=b log=p
                     proxy=q python=q resume-retries=i retries=i timeout=i trusted-host=b use-deprecated=b
                     use-feature=b].freeze,
          cache: %w[format=b].freeze,
          completion: %w[b|bash f|fish p|powershell z|zsh].freeze,
          config: %w[global user site editor=p].freeze,
          debug: %w[abi=b implementation=b platform=b python-version=b].freeze,
          download: %w[d|dest=p].freeze,
          freeze: %w[all exclude-editable l|local user exclude=b path=p r|requirement=p].freeze,
          index: %w[json].freeze,
          inspect: %w[local user path=p].freeze,
          install: %w[break-system-packages compile dry-run force-reinstall I|ignore-installed no-compile
                      no-warn-conflicts no-warn-script-location U|upgrade user prefix=p report=p root=p
                      root-user-action=b t|target=p upgrade-strategy=b].freeze,
          install_a: %w[ignore-requires-python no-index pre extra-index-url=q f|find-links=q i|index-url=q no-binary=q
                        only-binary=q].freeze,
          install_b: %w[build-constraint check-build-dependencies no-build-isolation no-clean no-deps prefer-binary
                        require-hashes use-pep517 c|constraint=p group=q progress-bar=b r|requirement=p src=p].freeze,
          install_c: %w[C|config-settings=q e|editable=v].freeze,
          hash: %w[a|algorithm].freeze,
          list: %w[e|editable exclude-editable include-editable l|local no-index not-required o|outdated pre u|uptodate
                   user exclude=b extra-index-url=q format=b f|find-links=q i|index-url=q path=p].freeze,
          lock: %w[o|output=p].freeze,
          show: %w[f|files].freeze,
          uninstall: %w[break-system-packages y|yes r|requirement=p root-user-action=b].freeze,
          wheel: %w[no-verify w|wheel-dir=p].freeze
        }.freeze
        OPT_POETRY = {
          common: %w[ansi no-ansi no-cache n|no-interaction no-plugins q|quiet=+ v|verbose=+ P|project=p].freeze,
          build: %w[clean config-settings=qq f|format=b o|output=p].freeze,
          publish: %w[build dry-run skip-existing cert=p client-cert=p dist-dir=p p|password=q r|repository=q
                      u|username=qq].freeze
        }.freeze
        OPT_PDM = {
          common: %w[I|ignore-python no-cache n|non-interactive].freeze,
          build: %w[C=bm no-clean no-isolation no-sdist no-wheel q|quiet v|verbose=+ config-setting=q d|dest=p
                    p|project=p k|skip=b].freeze,
          publish: %w[no-build no-very-ssl q|quiet S|sign skip-existing v|verbose=+ ca-certs=p c|comment=q d|dest=p
                      i|identity=b P|password=q p|project=p r|repository=q k|skip=b u|username=qq].freeze
        }.freeze
        OPT_HATCH = {
          common: %w[color interactive no-color no-interactive cache-dir=p config=p data-dir=p e|env=b p|project=b
                     q|quiet=+ v|verbose=+].freeze,
          build: %w[clean-hooks-after ext hooks-only no-hooks c|clean t|target=b].freeze,
          publish: %w[initialize-auth n|no-prompt y|yes a|auth=q ca-cert=p client-cert=p client-key=p o|option=q
                      p|publisher=b r|repo=q u|user=q].freeze
        }.freeze
        OPT_TWINE = {
          publish: %w[attestations disable-progress-bar non-interactive s|sign skip-existing verbose cert=p
                      client-cert=p c|comment=q config-file=p i|identity=b p|password=q r|repository=b repository-url=q
                      sign-with=b u|username=qq].freeze
        }.freeze
        PASS_PYTHON = {
          python: %w[c v V].freeze,
          pip: {
            debug: %w[platform].freeze,
            install: %w[C config-settings c constraint extra-index-url no-binary only-binary platform
                        r requirement].freeze,
            list: %w[exclude extra-index-url].freeze
          }.freeze
        }.freeze
        private_constant :DEP_PYTHON, :DIR_PYTHON, :OPT_PYTHON, :OPT_PIP, :OPT_POETRY, :OPT_PDM, :OPT_HATCH, :OPT_TWINE,
                         :PASS_PYTHON

        class << self
          def tasks
            [:outdated].freeze
          end

          def bannerargs
            %i[dependfile venv].freeze
          end

          def venv?
            Dir.exist?(ENV.fetch('VIRTUAL_ENV', ''))
          end

          def config?(val)
            return false unless (val = as_path(val))

            DIR_PYTHON.any? { |file| val.join(file).exist? }
          end
        end

        attr_reader :venv, :editable

        def initialize(*, editable: '.', asdf: 'python', **kwargs)
          super
          if @pass.include?(Python.ref)
            initialize_ref Python.ref
            initialize_logger(**kwargs)
          else
            initialize_build(Python.ref, **kwargs)
            initialize_env(**kwargs)
          end
          dependfile_set DEP_PYTHON
          editable_set editable
          venv_set kwargs[:venv]
        end

        subtasks({
          'venv' => %i[exec create remove show].freeze,
          'pip' => %i[upgrade uninstall wheel reinstall freeze].freeze,
          'install' => %i[user force upgrade target editable].freeze,
          'outdated' => %i[major minor patch].freeze,
          'build' => %i[poetry pdm hatch python].freeze,
          'publish' => %i[poetry pdm hatch twine].freeze,
          'run' => nil,
          'exec' => nil
        })

        def verbose=(val)
          case val
          when /\Av+\z/
            @verbose = val.size
          else
            super
          end
        end

        def ref
          Python.ref
        end

        def populate(*, **)
          super
          return unless (outdated? && ref?(Python.ref)) || @only

          namespace name do
            Python.subtasks do |action, flags|
              next if task_pass?(action)

              if flags.nil?
                case action
                when 'run'
                  next unless pyprojectfile

                  format_desc action, nil, "script+|#{indexchar}index+|#,pattern*"
                  task action, [:command] do |_, args|
                    found = 0
                    %w[tool.poetry.scripts tool.pdm.scripts project.scripts].each_with_index do |table, i|
                      next if (list = read_pyproject(table)).empty?

                      if args.command == '#'
                        format_list(list, "run[#{indexchar}N]", 'scripts', grep: args.extras, from: pyprojectfile)
                        found |= 1
                      else
                        args.to_a.each do |val|
                          if (n, = indexitem(val))
                            if (script, = list[n.pred])
                              case i
                              when 0
                                script = session_output 'poetry', 'run', script
                              when 1
                                script = pdm_session 'run', script
                              else
                                venv_init
                              end
                              found |= 1
                              run(script, from: :run)
                            elsif exception
                              indexerror n, list
                            else
                              found |= 2
                              log.warn "run script #{n} of #{list.size}".subhint('out of range')
                            end
                          else
                            case i
                            when 0
                              found |= 1
                              run(session_output('poetry', 'run', val), from: :run)
                            when 1
                              found |= 1
                              run(pdm_session('run', val), from: :run)
                            else
                              raise_error "script: #{val}" if exception
                              found |= 2
                              log.warn "run script \"#{val}\"".subhint('not indexed')
                            end
                          end
                        end
                      end
                      break
                    end
                    next if found.anybits?(1)

                    puts log_message(found == 0 ? Logger::INFO : Logger.WARN,
                                     "no scripts #{found == 0 ? 'found' : 'executed'}",
                                     subject: name, hint: pyprojectfile)
                  end
                when 'exec'
                  format_desc action, nil, ':|command,args*'
                  task action do |_, args|
                    args = args.to_a
                    cmd = if (i = args.delete(':')) && !workspace.windows?
                            readline('Enter script', force: true, multiline: %w[## ;])
                          elsif i || args.empty?
                            readline('Enter command', force: true)
                          else
                            (args << command_args(args, min: 1, prefix: 'python')).compact.join(' ')
                          end
                    shell(cmd, name: :exec, chdir: path)
                  end
                end
              else
                namespace action do |ns|
                  flags.each do |flag|
                    case action
                    when 'venv'
                      if flag == :create
                        format_desc action, flag, 'dir,opts*'
                        task flag, [:dir] do |_, args|
                          dir = basepath param_guard(action, flag, args: args, key: :dir)
                          venv_create dir, args.extras
                        end
                      elsif venv
                        case flag
                        when :remove
                          next unless projectpath?(venv)

                          format_desc action, flag, 'c/reate?,d/epend?,opts*'
                          task flag do |_, args|
                            args = args.to_a
                            rm_rf(venv, verbose: true)
                            venv_init if has_value!(args, 'c', 'create')
                            depend :force, args if has_value!(args, 'd', 'depend')
                          end
                        when :exec
                          format_desc action, flag, 'command,args*'
                          task flag do |_, args|
                            args = args.to_a
                            if args.empty?
                              args = readline('Enter command', force: true).split(' ', 2)
                            elsif args.size == 1 && !option('interactive', equals: '0', prefix: ref)
                              args << readline('Enter arguments', force: false) unless args.first.include?(' ')
                            end
                            venv_init
                            run args.join(' ')
                          end
                        when :show
                          format_desc action, flag
                          task flag do
                            puts venv
                          end
                        end
                      end
                    when 'pip'
                      case flag
                      when :upgrade
                        format_desc action, flag, 'opts*'
                        task flag do |_, args|
                          install flag, ['upgrade', *args.to_a, 'pip']
                        end
                      when :freeze
                        format_desc action, flag, "file?=#{DEP_PYTHON[4]},opts*"
                        task flag do |_, args|
                          if (file = pip(flag, opts: args.to_a, banner: true)) && !silent?
                            puts File.read(file)
                          end
                        end
                      when :uninstall
                        format_desc action, flag, 'package+,opts*'
                        task flag do |_, args|
                          pip(flag, opts: args.to_a, banner: true)
                        end
                      when :wheel
                        next unless pyprojectfile || setuptools?

                        format_desc action, flag, 'opts*,args*'
                        task flag do |_, args|
                          pip(flag, opts: args.to_a, banner: true)
                        end
                      when :reinstall
                        next unless venv && projectpath?(venv)

                        format_desc action, flag
                        task flag do
                          ns['venv:remove'].invoke('depend')
                        end
                      end
                    when 'install'
                      format_desc(action, flag, 'opts*', before: case flag
                                                                 when :target then 'dir'
                                                                 when :editable then 'path/url?,opts*'
                                                                 when :upgrade then 'strategy?,opts*,package+'
                                                                 end)
                      case flag
                      when :editable
                        task flag do |_, args|
                          install flag, args.to_a
                        end
                      when :upgrade
                        task flag, [:strategy] do |_, args|
                          install flag, (case args.strategy
                                         when 'eager'
                                           'eager'
                                         when /^only-if|needed$/
                                           'only-if-needed'
                                         end.yield_self do |val|
                                           if val
                                             args.extras << "upgrade-strategy=#{val}"
                                           else
                                             args.to_a
                                           end
                                         end)
                        end
                      when :target
                        task flag, [:dir] do |_, args|
                          dir = param_guard(action, flag, args: args, key: :dir)
                          depend(flag, args.extras, target: dir)
                        end
                      else
                        task flag do |_, args|
                          depend flag, args.to_a
                        end
                      end
                    when 'outdated'
                      format_desc(action, flag, "eager?,no-deps?,#{shortname('h', 'i', 's', 'u', 'd')}",
                                  before: ('user?' unless venv))
                      task flag do |_, args|
                        outdated flag, args.to_a
                      end
                    when 'build'
                      next if (be = backend?(flag)) == false

                      format_desc(action, flag, 'opts*', after: case flag
                                                                when :poetry then 'output?'
                                                                when :pdm then 'dest?'
                                                                when :hatch then 'location?'
                                                                else 'outdir?,srcdir?'
                                                                end)
                      task flag do |_, args|
                        build! flag, args.to_a
                      end
                      break if be
                    when 'publish'
                      next if (be = backend?(flag)) == false

                      format_desc(action, flag, 'test?,opts*', after: case flag
                                                                      when :hatch then 'artifacts?'
                                                                      when :twine then 'dist?'
                                                                      end)
                      task flag do |_, args|
                        args = args.to_a
                        publish(flag, args, test: if args.first == 'test'
                                                    args.shift
                                                    true
                                                  else
                                                    false
                                                  end)
                      end
                      break if be
                    end
                  end
                end
              end
            end
          end
        end

        def depend(flag = nil, opts = [], sync: invoked_sync?('depend', flag), target: nil, **)
          if @depend && !flag
            super
          elsif outdated?
            venv_init
            workspace.rev_clear(name, sync: sync)
            if !flag && poetry?
              cmd = poetry_session 'install -n'
              cmd << '--no-root' if option('no-root')
            else
              cmd = pip_session 'install'
              cmd << '--upgrade-strategy=eager' if env('PYTHON_UPDATE')
              if flag
                case flag
                when :user
                  cmd << '--user'
                when :target
                  cmd << quote_option('target', basepath(target))
                when :force
                  cmd << '--force-reinstall'
                end
                op = append_pip(flag, opts, from: :install)
                op.clear
              else
                append_global
              end
              cmd << "-r #{DEP_PYTHON[4]}" if exist?(DEP_PYTHON[4]) && !session_arg?('r', 'requirement')
              append_editable
            end
            run(sync: sync, from: :depend)
          end
        end

        def outdated(flag = nil, opts = [], sync: invoked_sync?('outdated', flag))
          cmd = pip_session 'list --outdated'
          cmd << if flag
                   se = has_value! opts, 's', 'select'
                   ia = has_value!(opts, 'i', 'interactive') && !se
                   up = has_value! opts, 'u', 'update'
                   hide = has_value! opts, 'h', 'hide'
                   dryrun = has_value! opts, 'd', 'dry-run'
                   if !sync || stdin?
                     se = false
                     ia = false
                   elsif se || ia
                     up = true
                     items = []
                   end
                   '--not-required' if opts.include?('no-deps')
                 else
                   if (up = option('u', 'update'))
                     flag = case up
                            when 'major', 'minor'
                              up.to_sym
                            else
                              :patch
                            end
                   end
                   '--not-required' unless option('not-required', equals: '0')
                 end
          cmd << '--local' if option('l', 'local')
          append_global
          dryrun ||= dryrun?
          cmd = session_done cmd
          log.info cmd
          on :first, :outdated
          banner = format_banner cmd
          print_item banner if sync
          pwd_set(from: :outdated) do
            tc = theme[:current]
            start = 0
            found = 0
            col = 0
            major = []
            minor = []
            patch = []
            buffer = []
            out = ->(val) { sync ? puts(val) : buffer << val }
            if workspace.windows?
              (venv ? command(runenv, cmd) : `#{cmd}`).lines(chomp: true)
            else
              IO.popen(runenv || {}, cmd).readlines(chomp: true)
            end.each do |line|
              next if line.match?(/^[ -]+$/)

              if start > 0
                n = line.size
                unless stdin?
                  cur, lat = line.scan(SEM_VER)
                  next unless cur && lat

                  name = line.split(' ', 2).first
                  c = cur.join
                  l = lat.join
                  semver cur
                  semver lat
                  case (type = semtype(cur, lat))
                  when 1
                    major << name
                  when 2
                    minor << name
                  else
                    patch << name
                  end
                  next if hide && ((flag == :patch && type < 3) || (flag == :minor && type < 2))

                  if type == 3
                    styles = color(:yellow)
                  else
                    styles = color(:green)
                    sub_style!(line, if type == 1
                                       styles += [:bold]
                                       theme[:major]
                                     else
                                       theme[:active]
                                     end, pat: /^(\S+)(.+)$/)
                  end
                  sub_style!(line, **opt_style(tc, /^(.+)(#{Regexp.escape(c)})(.+)$/, 2)) if tc
                  sub_style!(line, **opt_style(styles, /^(.+)(#{Regexp.escape(l)})(.+)$/, 2))
                  found += 1
                end
                s = '%2d. %s' % [start, line]
                start += 1
                if ia
                  next unless confirm_semver(s.ljust(col + line.size - n), type)
                elsif !se
                  out.call(s)
                end
                items&.push([line, name])
              elsif line.start_with?('Package')
                unless stdin?
                  col = line.size + 5
                  sub = [opt_style(theme[:header], /^(.*)(?<!\dm)(Package|Latest)(.+)$/, 2)] * 2
                  out.call(print_footer(" #  #{line}", reverse: true, sub: sub))
                end
                start += 1
              end
            end
            unless sync
              print_item banner
              puts buffer
            end
            if found > 0
              items = if se
                        choice('Select a package', items.map(&:first),
                               multiple: true, force: false, index: true, border: true).map! { |n| items[n.pred].last }
                      elsif ia
                        items.map(&:last)
                      else
                        case flag
                        when :major
                          major + minor + patch
                        when :minor
                          minor + patch
                        else
                          patch
                        end
                      end
              if up && !items.empty?
                base = %w[eager no-deps]
                base << 'user' unless venv
                opts = (base & opts).map! { |val| val == 'eager' ? "upgrade-strategy=#{val}" : val }
                if dryrun
                  opts.map! { |val| fill_option(val) }
                  print_run pip_output('install --upgrade', *opts, *items.quote!), false
                else
                  install(:upgrade, opts, packages: items, banner: false)
                end
              end
              print_status(major.size, minor.size, patch.size, from: :outdated)
            elsif start == 0 || hide
              puts 'No updates were found'
            end
          end
          on :last, :outdated
        end

        def install(flag, opts = [], packages: [], banner: true)
          op = append_pip(flag, opts, target: pip_session('install'), from: :install)
          case flag
          when :editable
            op << quote_option('e', op.pop || editable || '.')
            op.clear
          when :upgrade
            op.concat(packages)
            raise_error 'no packages listed', hint: flag if op.empty?
            op << '--upgrade'
            op.append
            python_session('-m pip', *op.to_a.drop(1)) if workspace.windows?
          end
          run(banner: banner, from: :install)
        end

        def build!(flag, opts = [])
          list = case flag
                 when :poetry
                   cmd = poetry_session 'build'
                   OPT_POETRY[:build] + OPT_POETRY[:common]
                 when :pdm
                   cmd, opts = pdm_session('build', opts: opts)
                   OPT_PDM[:build]
                 when :hatch
                   cmd, opts = hatch_session('build', opts: opts)
                   OPT_HATCH[:build]
                 else
                   cmd, opts = python_session('-m build', opts: opts)
                   OPT_PYTHON[:build]
                 end
          op = OptionPartition.new(opts, list, cmd, project: self, single: singleopt(flag))
          case flag
          when :hatch
            if !ENV['HATCH_BUILD_LOCATION'] && (outdir ||= op.shift)
              op.add_path(outdir)
            end
          else
            unless op.empty?
              args = case flag
                     when :poetry
                       %w[o output]
                     when :pdm
                       %w[d dest]
                     else
                       srcdir = true
                       %w[o outdir]
                     end
              op << quote_option(args.last, basepath(op.shift)) unless op.arg?(*args)
            end
          end
          op.exist?(add: true, first: true) if srcdir
          op.clear
          run(from: :"#{flag}:build")
        end

        def publish(flag, opts = [], test: false)
          list = case flag
                 when :poetry
                   poetry_session 'publish'
                   OPT_POETRY[:publish] + OPT_POETRY[:common]
                 when :pdm
                   opts = pdm_session('publish', opts: opts).last
                   OPT_PDM[:publish]
                 when :hatch
                   opts = hatch_session('publish', opts: opts).last
                   OPT_HATCH[:publish]
                 else
                   session 'twine', 'upload'
                   OPT_TWINE[:publish]
                 end
          op = OptionPartition.new(opts, list, @session, project: self, single: singleopt(flag))
          dist = lambda do
            dir = basepath 'dist'
            return dir if dir.directory? && !dir.empty?

            if dir.exist?
              raise_error 'no source to publish', hint: dir
            else
              raise_error Errno::ENOENT, dir, hint: 'publish'
            end
          end
          if test
            if op.arg?('r', flag == :hatch ? 'repo' : 'repository')
              op.push('test')
            else
              op << quote_option('r', 'testpypi')
            end
          end
          case flag
          when :poetry, :pdm
            dist.call unless op.arg?(*(flag == :poetry ? ['dist-dir'] : %w[d dest]))
            op.clear(pass: false)
          else
            if op.empty?
              op << "#{dist.call}/*"
            else
              op.add_path
            end
          end
          run(from: :"#{flag}:publish", interactive: ['Publish', 'N', project])
        end

        def python(*args, sync: true, banner: verbose?, with: nil, pass: PASS_PYTHON[:python], **kwargs)
          op = OptionPartition.new(session_opts(with, args: args, kwargs: kwargs, pass: pass), OPT_PYTHON[:common],
                                   session('python', path: venv.nil?),
                                   project: self, multiple: [/^-c/], single: singleopt(:python), args: true,
                                   stdin: true)
          op.concat(args)
          if op.include?('-')
            op.exist?(add: true)
          else
            op.append_any { |val| OptionPartition.parse_arg!('c', val) }
            if op.arg?('c')
              op.clear
            else
              op.exist?(add: true, first: true) unless op.arg?('m')
              op.append(escape: kwargs.fetch(:escape, false), quote: kwargs.fetch(:quote, false))
            end
          end
          print_run(op, banner, **kwargs)
          run(sync: sync, banner: banner, exception: kwargs.fetch(:exception, exception), from: :python)
        end

        def pip(flag, *args, sync: true, banner: verbose?, with: nil, pass: nil, **kwargs)
          flag = flag.to_sym
          pass = PASS_PYTHON[:pip].fetch(pip_install?(flag) ? :install : flag, []) + %w[v verbose] if pass.nil?
          opts = session_opts(with, args: args, kwargs: kwargs, pass: pass)
          case flag
          when :freeze, :inspect, :list, :check, :completion, :debug
            opts.concat(args)
          end
          op = append_pip(flag, opts, target: pip_session(flag), from: flag)
          case flag
          when :install, :uninstall, :show, :index
            op.concat(args)
            if op.empty?
              case flag
              when :install, :uninstall
                op << '.' if installable? && !op.arg?('r', 'requirement')
              else
                raise_error 'no packages listed', hint: flag
              end
            elsif flag == :install
              op.append_any
            elsif flag == :index
              op.adjoin('versions', with: 'index')
                .add_first
                .clear
            else
              op.append
            end
          when :freeze
            venv_init
            op << '>'
            op.add_quote(ret = basepath(op.detect { |val| op.exist?(val) } || DEP_PYTHON[4]))
              .clear
          when :cache
            op.concat(args)
            raise_error 'no subcommand', hint: flag if op.empty?
            op << (action = op.shift)
            case action
            when 'dir', 'info', 'purge'
              nil
            when 'list', 'remove'
              op.add_first(quote: true)
            else
              raise_error ArgumentError, "unrecognized args: #{action}", hint: flag
            end
            op.clear
          when :config
            op.concat(args)
            raise_error 'no subcommand', hint: flag if op.empty?
            op << (action = op.shift)
            case action
            when 'list', 'edit', 'debug'
              nil
            when 'get', 'unset', 'set'
              op.add_first
              op.add_first(quote: true, expect: true) if action == 'set'
            else
              raise_error ArgumentError, "unrecognized args: #{action}", hint: flag
            end
            op.clear
          when :hash
            op.append(projectmap(op.concat(args), parent: true))
          when :wheel, :lock, :download
            op.concat(args)
            if !op.empty?
              op.append
            elsif installable? && !op.arg?('r', 'requirement')
              op << '.'
            end
          else
            op.clear
          end
          print_run(op, banner, **kwargs)
          run(sync: sync, banner: banner, exception: kwargs.fetch(:exception, exception), from: :"pip:#{flag}")
            .yield_self { |val| ret || val }
        end

        def variable_set(key, *args, **, &blk)
          if block_given?
            case key
            when :dependfile, :venv, :editable
              args = block_args args, &blk
            end
          end
          case key
          when :dependfile
            if args.first.nil?
              super
            else
              val = basepath(*args)
              if (index = DEP_PYTHON.index(val.basename.to_s))
                @dependindex = index
                @dependfile = val
              else
                log.warn "variable_set: @#{key}=#{val} (not supported)"
              end
            end
          when :editable
            editable_set args.first
          when :venv
            @venv = args.empty? || args.first.nil? ? nil : basepath(*args)
          else
            super
          end
        end

        def depend?
          @depend != false && (!@depend.nil? || outdated?)
        end

        def outdated?
          dependtype > 0 && !task_pass?('outdated')
        end

        private

        def pip_session(*cmd)
          session('pip', *cmd, *preopts, path: venv.nil?)
        end

        def pip_output(*cmd)
          session_output('pip', *cmd, *preopts, path: venv.nil?)
        end

        def python_session(*cmd, opts: nil)
          pre = preopts(quiet: false)
          return session('python', *pre, *cmd, path: venv.nil?) unless opts

          op = OptionPartition.new(opts, OPT_PYTHON[:common], project: self, single: singleopt(:python))
          [session('python', *pre, *op.to_a, *cmd, path: venv.nil?), op.extras]
        end

        def poetry_session(*cmd)
          ret = session('poetry', *cmd, *preopts)
          option('project', ignore: false) { |val| ret << quote_option('project', basepath(val)) }
          ret
        end

        def pdm_session(*cmd, opts: nil)
          create_session(*cmd, name: 'pdm', common: OPT_PDM[:common], opts: opts)
        end

        def hatch_session(*cmd, opts: nil)
          create_session(*cmd, name: 'hatch', common: OPT_HATCH[:common], opts: opts)
        end

        def create_session(*cmd, name:, common:, opts: nil)
          return session(name, *preopts, *cmd, path: venv.nil?) unless opts

          op = OptionPartition.new(opts, common, project: self, single: singleopt(name.to_sym))
          [session(name, *preopts, *op.to_a, *cmd, path: venv.nil?), op.extras]
        end

        def append_pip(flag, opts, target: @session, from: nil)
          list = OPT_PIP.fetch(from, []) + OPT_PIP[:common]
          if pip_install?(flag) || from == :install
            list.concat(OPT_PIP[:install_a])
            list.concat(OPT_PIP[:install_b]) unless flag == :index
            case flag
            when :install, :editable, :upgrade
              list.concat(OPT_PIP[:install_c] + OPT_PIP[:debug])
            when :lock, :wheel
              list.concat(OPT_PIP[:install_c])
            when :download, :index
              list.concat(OPT_PIP[:debug])
            end
            opts << 'no-build-isolation' if option('build-isolation', equals: '0')
          end
          op = OptionPartition.new(opts, list, target, project: self, single: singleopt)
          append_global(target: target)
          case flag
          when :install, :lock, :wheel, :editable, :upgrade
            edit = nil
            op.each do |opt|
              if opt =~ op.values
                case $1
                when 'e', 'editable'
                  op.found << edit if edit && flag == :editable
                  edit = $2
                end
              elsif flag == :editable && !edit
                edit = opt
              else
                op.found << opt
              end
            end
            op.swap
            if edit
              edit = basepath(edit) unless %r{\A[a-z]+(?:\+[a-z]+)?://}i.match?(edit)
              if flag == :editable
                op.push(edit)
              else
                op << quote_option('e', edit)
              end
            end
          end
          op
        end

        def append_editable(target: @session)
          return if requirements? && editable == '.'

          if (val = option('e', 'editable', target: target, ignore: false))
            OptionPartition.delete_key(target, 'e', 'editable')
            case val
            when '0', 'false'
              return unless installable?
            else
              val = basepath val
            end
          elsif session_arg?('e', 'editable', target: target) || !installable?
            return
          else
            val = editable
          end
          target << (val ? quote_option('e', basepath(val)) : '.')
        end

        def append_global(target: @session)
          target.merge([
            option('cache-dir', target: target) do |val|
              case val
              when '0', 'false'
                '--no-cache-dir'
              else
                quote_option 'cache-dir', basepath(val)
              end
            end,
            option('proxy', target: target) { |val| quote_option('proxy', val) },
            option('python', target: target) { |val| quote_option('python', basepath(val)) }
          ])
          append_nocolor(target: target)
        end

        def build_backend
          @build_backend ||= read_pyproject('build-system', 'build-backend') || ''
        end

        def read_pyproject(table, key = nil)
          return [] unless (file = pyprojectfile)

          unless (ret = (@pyproject ||= {})[table])
            ret = []
            start = /^\s*\[#{Regexp.escape(table)}\]\s*$/
            ch = nil
            found = false
            File.foreach(file) do |line|
              if found
                break if line.match?(/^\s*\[[\w.-]+\]\s*$/)

                if ch
                  val = line.rstrip
                  case ch
                  when '}', ']'
                    ch = nil if val.end_with?(ch)
                    val = "\n#{val}"
                  else
                    if val.chomp!(ch)
                      ch = nil
                    else
                      val = line
                    end
                  end
                  ret.last[1] += val
                elsif (data = line.match(/^\s*(\S+)\s*=\s*([+-]?[\d.]+|true|false|("""|'''|["'\[{])(.*?))\s*$/))
                  if (val = data[4])
                    case (ch = data[3])
                    when '{', '['
                      val = "#{ch}#{val}"
                      ch = ch == '{' ? '}' : ']'
                      ch = nil if val.end_with?(ch)
                    else
                      if val.chomp!(ch)
                        ch = nil
                      elsif ch.size == 1
                        next
                      end
                    end
                  else
                    val = case (ch = data[2])
                          when 'true'
                            true
                          when 'false'
                            false
                          else
                            ch.include?('.') ? ch.to_f : ch.to_i
                          end
                  end
                  ret << [data[1], val]
                end
              else
                found = line.match?(start)
              end
            end
            @pyproject[table] = ret
          end
          return ret.find { |val| val.first == key }&.last if key

          ret
        end

        def pyprojectfile
          @pyprojectfile = basepath!(DEP_PYTHON[2]) || false if @pyprojectfile.nil?
          @pyprojectfile || nil
        end

        def singleopt(flag = nil)
          case flag
          when :python
            /\A(?:v+|q+|b+|V+|O+)\z/
          when :pdm
            /\Av+\z/
          when :twine
            nil
          else
            /\A(?:v+|q+)\z/
          end
        end

        def preopts(quiet: true)
          ret = []
          case verbose
          when FalseClass
            ret << '--quiet' if quiet
          when Numeric
            ret << "-#{'v' * verbose}" if verbose > 0
          end
          ret
        end

        def variables
          (super + %i[venv editable]).freeze
        end

        def runenv
          return unless venv

          if workspace.windows?
            shell_quote(venvbin.join(workspace.powershell? ? 'Activate.ps1' : 'activate.bat'), option: false)
          else
            { 'VIRTUAL_ENV' => venv.to_s, 'PATH' => "#{venvbin}:#{ENV['PATH']}" }
          end
        end

        def venvbin
          @venv&.join(workspace.windows? ? 'Scripts' : 'bin')
        end

        def editable_set(val)
          @editable = case val
                      when '.', Pathname
                        val
                      when String
                        Pathname.new(val) unless val.empty?
                      end
        end

        def venv_set(val)
          return unless val

          write = ->(level, hint) { log.add(level, "venv: #{@venv}".subhint(hint)) }
          if val.is_a?(Array)
            val, *opts = val
            @venvopts = opts
          end
          @venv = basepath val
          if projectpath?(@venv)
            if @venv.exist?
              write.call(Logger::DEBUG, 'found')
            elsif path.directory? && !path.empty?
              @venv.mkpath
              write.call(Logger::INFO, 'mkdir')
            end
          elsif !@venv.directory?
            write.call(Logger::WARN, 'invalid')
            @venv = nil
          end
        end

        def venv_init
          return if !venv || (venvbin.directory? && !venvbin.empty?)

          puts log_message(venv, subject: 'venv', hint: 'init') unless silent?
          opts = @venvopts&.map { |val| OptionPartition.strip(val) }&.flatten
          venv_create(venv, opts || ["prompt=#{name}", 'upgrade-deps'], env: false, banner: false)
          puts log_message(venv, subject: 'venv', hint: 'created') unless silent?
        end

        def venv_create(dir, opts = [], env: nil, banner: banner?)
          cmd, opts = python_session('-m venv', opts: opts)
          op = OptionPartition.new(opts, OPT_PYTHON[:venv], cmd, project: self)
          status = op.append(dir, delim: true)
                     .clear(pass: false)
                     .arg?(/\A-v+\z/)
          ret = run(op, env, exception: true, banner: banner)
          pip(:install, 'poetry', banner: false) if poetry?
          success?(ret, banner, !status) { |out| puts(out && dir.directory? ? "Success: #{dir}" : 'Failed') }
        end

        def pip_install?(flag)
          %i[install download index lock wheel].include?(flag)
        end

        def backend?(flag)
          case flag
          when :poetry
            build_backend == 'poetry.core.masonry.api'
          when :pdm
            build_backend == 'pdm.backend'
          when :hatch
            build_backend == 'hatchling.build'
          when :setuptools
            build_backend == 'setuptools.build_meta'
          end
        end

        def installable?
          setuptools? || !!pyprojectfile
        end

        def setuptools?
          dependtype == 2 || dependtype == 4
        end

        def poetry?
          dependtype == 1
        end

        def requirements?
          dependtype == 5
        end
      end

      Application.implement Python

      class Ruby < Git
        GEMFILE = %w[Gemfile Gemfile.lock gem.deps.rb gems.rb Isolate].freeze
        GEMNAME = /\A[A-Za-z\d][A-Za-z\d_.-]*\z/.freeze
        DIR_RUBY = (GEMFILE + Rake::Application::DEFAULT_RAKEFILES + ['README.rdoc']).freeze
        OPT_RUBY = {
          ruby: %w[0=im? a c C=pm e=q E=bm F=qm i=bm? I=pm l n p r=bm s S w W=bm? x=pm? d|debug jit rjit verbose
                   y|yydebug backtrace-limit=i crash-report=q disable=q dump=q enable=q encoding=b external-encoding=b
                   internal-encoding=b parser=b].freeze,
          rake: %w[A|all B|build-all comments n|dry-run m|multitask P|prereqs q|quiet X|no-deprecation-warnings
                   N|no-search G|no-system nosearch nosystem rules s|silent g|system v|verbose backtrace=b?
                   D|describe=q? e|execute=q E|execute-continue=q p|execute-print=q f|rakefile=p job-stats=b? j|jobs=i?
                   I|libdir=p R|rakelib=p rakelibdir=p r|require=b suppress-backtrace=q T|tasks=q? t|trace=b?
                   W|where=q?].freeze,
          irb: %w[d f U w E=b I=p r=b W=im? autocomplete colorize echo echo-on-assignment extra-doc-dir inf-ruby-mode
                  inspect multiline no-pager noautocomplete nocolorize noecho noecho-on-assignment noinspect
                  nomultiline noprompt noscript nosingleline noverbose regexp-completor sample-book-mode script
                  simple-prompt single-irb singleline tracer truncate-echo-on-assignment type-completor verbose
                  back-trace-limit=i context-mode=i prompt=b prompt-mode=b].freeze,
          rbs: %w[I=pm r=bm no-stdlib no-collection collection=p log-level=b log-output=p repo=p].freeze,
          rubocop: %w[D P r=bm auto-gen-config a|autocorrect A|autocorrect-all d|debug disable-pending-cops
                      display-only-correctable display-only-fail-level-offenses display-only-failed
                      display-only-safe-correctable S|display-style-guide display-time editor-mode enable-pending-cops
                      E|extra-details F|fail-fast force-default-config force-exclusion x|fix-layout
                      ignore-disable-comments ignore-parent-exclusion ignore-unrecognized-cops init l|lint
                      L|list-target-files lsp memory no-detach only-guide-cops only-recognized-file-types
                      no-exclude-limit profile raise-cop-error regenerate-todo restart-server safe server-status
                      start-server stderr stop-server C|cache=b cache-root=p config=p exclude-limit=i fail-level=b
                      f|format=b except=q only=q o|out=p plugin=p require=p show-cops=q show-docs-url=q
                      s|stdin=p].freeze,
          no: {
            rubocop: %w[auto-gen-enforced-style auto-gen-only-exclude auto-gen-timestamp color display-cop-names
                        offense-counts parallel server].freeze
          }
        }.freeze
        OPT_BUNDLE = {
          common: %w[no-color V|verbose r|retry=i].freeze,
          common_all: %w[all all-platforms path=p].freeze,
          common_git: %w[branch=q git=q path=p ref=q].freeze,
          common_version: %w[local major minor patch pre strict].freeze,
          add: %w[optimistic quiet skip-install strict github=q glob=q g|group=q require=q s|source=q
                  v|version=q].freeze,
          binstubs: %w[force standalone shebang=q].freeze,
          cache: %w[frozen no-all no-install no-prune quiet cache-path=p gemfile=p].freeze,
          check: %w[dry-run gemfile=p path=p].freeze,
          clean: %w[dry-run force].freeze,
          config: %w[global local skip-parseable].freeze,
          doctor: %w[quiet ssl gemfile=p].freeze,
          doctor_ssl: %w[host=q tls-version=b verify-mode=b].freeze,
          exec: %w[gemfile=p].freeze,
          gem: %w[b|bin git no-exe rubocop ci=b e|edit=p? ext=b github-username=q linter=b t|test=b?].freeze,
          init: %w[gemfile=p gemspec=p].freeze,
          install: %w[frozen no-cache no-prune system binstubs=p? path=p standalone=q? target-rbconfig=p trust-policy=b
                      with=q without=q].freeze,
          install_a: %w[force full-index local quiet redownload gemfile=p j|jobs=i].freeze,
          lock: %w[add-checksums conservative full-index normalize-platforms print add-platform=q bundler=b? gemfile=p
                   lockfile=p remove-platform=p update=q?].freeze,
          open: %w[path=p].freeze,
          outdated: %w[filter-major filter-minor filter-patch filter-strict groups parseable porcelain only-explicit
                       update-strict group=q source=q].freeze,
          platform: %w[ruby].freeze,
          plugin: %w[source=q version=q].freeze,
          plugin_uninstall: %w[all].freeze,
          remove: %w[install].freeze,
          show: %w[outdated paths].freeze,
          update: %w[all conservative local major minor patch pre ruby strict bundler=b? g|group=q source=q].freeze,
          no: {
            config: %w[parseable].freeze,
            gem: %w[changelog ci coc exe linter mit test].freeze
          }.freeze
        }.freeze
        OPT_GEM = {
          common: %w[backtrace debug q|quiet no-verbose norc silent V|verbose config-file=p].freeze,
          common_url: %w[B|bulk-threshold=i p|http-proxy=q? s|source=q].freeze,
          common_domain: %w[b|both clear-sources l|local r|remote].freeze,
          common_otp: %w[host=q k|key=b otp=b].freeze,
          common_all: %w[a|all e|exact v|version=q].freeze,
          build: %w[C=p force strict o|output=p platform=q].freeze,
          cert: %w[a|add=p b|build=q C|certificate=p d|days=i l|list=q A|key-algorithm=b K|private-key=p r|remove=q
                   R|re-sign s|sign=p].freeze,
          check: %w[a v|version=q].freeze,
          cleanup: %w[D n d|dry-run].freeze,
          contents: %w[l all s|spec-dir=q v|version=q].freeze,
          dependency: %w[R pipe platform=q v|version=q].freeze,
          exec: %w[conservative g|gem=b v|version=q].freeze,
          fetch: %w[clear-sources platform=q v|version=q].freeze,
          generate_index: %w[update d|directory=p].freeze,
          info: %w[i I].freeze,
          install: %w[v|version=q].freeze,
          install_a: %w[E f w conservative default development development-all explain ignore-dependencies N|no-document
                        vendor n|bindir=p build-root=p document=b? g|file=p? i|install-dir=p platform=q
                        target-rbconfig=p? P|trust-policy=b without=q].freeze,
          list: %w[d i I].freeze,
          lock: %w[s].freeze,
          open: %w[e|editor=p v|version=q].freeze,
          outdated: %w[platform=q].freeze,
          owner: %w[a|add=q r|remove=q p|http-proxy=q?].freeze,
          pristine: %w[E all only-executables only-missing-extensions only-plugins n|bindir=p i|install-dir=p skip=b
                       v|version=q].freeze,
          push: %w[attestation=p p|http-proxy=q?].freeze,
          rdoc: %w[all v|version=q].freeze,
          rebuild: %w[C=p diff force strict gemspec=p original=p source=q].freeze,
          search: %w[d i I].freeze,
          signin: %w[otp=b host=q].freeze,
          sources: %w[f c|clear-all l|list u|update a|add=q p|http-proxy=q? r|remove=q].freeze,
          specification: %w[all marshal ruby yaml platform=q v|version=q].freeze,
          uninstall: %w[a D I x vendor n|bindir=p i|install-dir=p platform=q v|version=q].freeze,
          unpack: %w[spec target=p P|trust-policy=b v|version=q],
          update: %w[system=b?].freeze,
          which: %w[a g].freeze,
          yank: %w[platform=q v|version=q].freeze,
          no: {
            check: %w[alien doctor dry-run gems].freeze,
            cleanup: %w[check-development user-install].freeze,
            contents: %w[lib-only prefix show-install-dir].freeze,
            dependency: %w[http-proxy prerelease reverse-dependencies].freeze,
            exec: %w[prerelease].freeze,
            fetch: %w[http-proxy prerelease suggestions].freeze,
            generate_index: %w[compact modern].freeze,
            info: %w[http-proxy installed prerelease versions].freeze,
            install: %w[env-shebang force format-executable http-proxy lock minimal-deps post-install-message prerelease
                        suggestions user-install wrappers].freeze,
            list: %w[details http-proxy installed prerelease versions].freeze,
            lock: %w[strict].freeze,
            outdated: %w[http-proxy].freeze,
            owner: %w[http-proxy].freeze,
            pristine: %w[env-shebang extensions].freeze,
            push: %w[http-proxy].freeze,
            rdoc: %w[overwrite rdoc ri].freeze,
            search: %w[details http-proxy installed prerelease versions].freeze,
            sources: %w[force http-proxy].freeze,
            specification: %w[prerelease].freeze,
            uninstall: %w[abort-on-dependent all check-development executables force format-executable
                          ignore-dependencies user-install].freeze,
            which: %w[all gems-first].freeze
          }.freeze
        }.freeze
        PASS_RUBY = {
          ruby: %w[e I disable enable dump r s].freeze,
          rake: %w[I libdir r require].freeze,
          irb: %w[I r].freeze,
          rbs: %w[I r repo].freeze,
          rubocop: %w[format plugin r require].freeze,
          gem: {
            contents: %w[s spec-dir].freeze,
            dependency: %w[s source].freeze,
            install: %w[document s source without].freeze,
            pristine: %w[skip].freeze
          }.freeze,
          bundle: {
            install: %w[standalone with without].freeze,
            lock: %w[add-platform remove-platform update].freeze,
            update: %w[g group source].freeze
          }.freeze
        }.freeze
        private_constant :GEMFILE, :GEMNAME, :DIR_RUBY, :OPT_RUBY, :OPT_BUNDLE, :OPT_GEM, :PASS_RUBY

        class << self
          def tasks
            [:outdated].freeze
          end

          def bannerargs
            %i[dependfile gemname gemdir].freeze
          end

          def config?(val)
            return false unless (val = as_path(val))

            DIR_RUBY.any? { |file| val.join(file).exist? }
          end
        end

        subtasks({
          'outdated' => %i[major minor patch].freeze,
          'ruby' => %i[file script version].freeze,
          'gem' => %i[install uninstall outdated update pristine build push exec command].freeze,
          'bundle' => %i[install update cache exec config reinstall command].freeze,
          'rake' => nil,
          'irb' => nil,
          'rbs' => nil,
          'rubocop' => nil
        })

        attr_reader :gemdir

        def initialize(*, autodetect: false, gemspec: nil, steep: 'Steepfile', rubocop: '.rubocop.yml', asdf: 'ruby',
                       **kwargs)
          super
          if @pass.include?(Ruby.ref)
            initialize_ref Ruby.ref
            initialize_logger(**kwargs)
          else
            initialize_build(Ruby.ref, **kwargs)
            initialize_env(**kwargs)
          end
          dependfile_set GEMFILE
          @autodetect = autodetect
          @gemfile = if gemspec == false
                       false
                     elsif gemspec
                       basepath(gemspec.include?('.') ? gemspec : "#{gemspec}.gemspec")
                     end
          @steepfile = basepath! steep if steep
          @rubocopfile = Pathname.new(rubocop).realpath rescue basepath!(Dir.home, '.rubocop.yml') if rubocop
          return unless rakefile && @output[0].nil? && @copy.nil? && !version && !@autodetect

          begin
            File.foreach(rakefile) do |line|
              next unless line.match?(%r{\brequire\s+(["'])bundler/gem_tasks\1})

              cmd = bundle_output('exec rake').to_s
              @output[0] = "#{cmd} build"
              @copy = "#{cmd} install"
              @clean = "#{cmd} clean" if @clean.nil?
              break
            end
          rescue StandardError => e
            log.error e
          end
        end

        def project=(val)
          @project = val.dup
        end

        def gemdir=(val)
          @gemdir = if val.is_a?(Pathname)
                      val
                    else
                      Pathname.new(val).realdirpath rescue nil
                    end
        end

        def ref
          Ruby.ref
        end

        def populate(*, **)
          super
          return unless (outdated? && ref?(Ruby.ref)) || @only

          namespace name do
            Ruby.subtasks do |action, flags|
              next if task_pass?(action)

              if flags.nil?
                case action
                when 'rake'
                  next unless rakefile

                  format_desc action, nil, "task+|#{indexchar}index+,opts*|#,pattern*"
                  task action, [:command] do |_, args|
                    if args.command == '#'
                      format_list(raketasks, "rake[#{indexchar}N]", 'tasks', grep: args.extras, from: rakefile, &:join)
                    else
                      tasks = raketasks
                      cmd = nil
                      opts = []
                      queue = lambda do
                        if cmd
                          cmd += "[#{opts.join(',')}]" unless opts.empty?
                          rake(cmd, banner: true)
                          cmd = nil
                        elsif !opts.empty?
                          rake(banner: true, opts: opts)
                        end
                        opts.clear
                      end
                      args.to_a.each do |item|
                        if (n, pre = indexitem(item))
                          queue.call
                          if (item = tasks[n.pred])
                            cmd = [pre, item.first].compact.join(' ')
                          elsif exception
                            indexerror n, tasks
                          else
                            log.warn "rake task #{n} of #{tasks.size} (out of range)"
                            opts.clear
                            next
                          end
                        else
                          opts << item
                        end
                      end
                      queue.call
                    end
                  end
                when 'irb'
                  format_desc action, nil, 'opts*,args*|:'
                  task action do |_, args|
                    opts = args.to_a
                    args = Array(opts.delete(':') && readline('Enter file [arguments]', force: false))
                    name = gemname if gemlib.any? { |file| exist?(file, "#{gemname}.rb") }
                    irb(*args, opts: opts, name: name, verbose: false)
                  end
                when 'rbs'
                  next unless @steepfile

                  data = {}
                  target = nil
                  File.foreach(@steepfile) do |line|
                    if line =~ /^\s*target(?:\s+|\(\s*)(?::(\S+)|(["'])(.+)\2)/
                      target = [[], []]
                      data[$1 || $3.gsub(/[: ]/, '-')] = target
                      next
                    end
                    next unless target && line =~ /^\s*(check|signature)\s+(["'])(.+)\2/

                    target[$1 == 'check' ? 1 : 0] << $3
                  end
                  next if data.empty?

                  namespace 'rbs' do
                    data.each do |key, item|
                      sig, lib = item
                      next if sig.empty? || lib.empty?

                      format_desc action, key, 'sig?,path*'
                      task key do |_, args|
                        args = args.to_a
                        list = lib.flat_map do |val|
                          val = File.join(val, '**/*.rb') unless val.include?('*') || val.match?(/\.[a-z\d]+$/i)
                          Dir.glob(val, base: path)
                        end
                        files = if args.empty?
                                  choice_index('Select files', list, multiple: true, series: true,
                                                                     accept: [accept_y('Generate?')])
                                else
                                  list.map! { |val| basepath(val).to_s }
                                  [].tap do |out|
                                    ret = []
                                    args.each do |val|
                                      if val.include?('*')
                                        ret.concat(Dir.glob(val, base: path))
                                      elsif !(file = basepath!(val))
                                        print_error(val, hint: 'not found')
                                      elsif file.directory?
                                        ret.concat(file.glob('**/*.rb'))
                                      else
                                        ret << val
                                      end
                                    end
                                    ret = ret.select { |val| list.include?(basepath(val).to_s) }
                                    if ret.empty?
                                      print_error('steep', 'no files matched', hint: "#{key}:check")
                                      exit 1
                                    end
                                    out.replace(ret.uniq)
                                  end
                                end
                        sig = if (n = sig.index(args.first))
                                args.shift
                                sig[n]
                              elsif sig.size > 1
                                choice_index('Select a sig', sig, series: true)
                              else
                                sig.first
                              end
                        rbs(:prototype, sig, *files)
                      end
                    end
                  end
                when 'rubocop'
                  next unless @rubocopfile

                  format_desc action, nil, 'opts*,path*/:'
                  task action do |_, args|
                    opts, args = args.to_a.partition do |val|
                      next true if val.match?(/\A(?:(?:[A-Z]|[a-z-]+)=.|[a-z]+(?:-[a-z]+)*\Z)/)

                      !val.include?('*') && !val.end_with?('/')
                    end
                    if opts.delete(':')
                      args << (Dir.exist?('lib') ? 'lib/' : '**/*.rb') if args.empty?
                      list = args.map! { |val| val.end_with?('/') || Dir.exist?(val) ? File.join(val, '**/*.rb') : val }
                                 .flat_map { |val| Dir.glob(val, base: path) }
                      args = choice_index('Select files', list, multiple: true)
                    end
                    rubocop(*args, opts: opts, banner: true)
                  end
                end
              else
                namespace action do
                  flags.each do |flag|
                    case action
                    when 'outdated'
                      format_desc action, flag, "#{shortname('i', 's', 'u', 'd')},e/xplicit,opts*"
                      task flag do |_, args|
                        outdated flag, args.to_a
                      end
                    when 'gem'
                      case flag
                      when :outdated
                        format_desc action, flag, "semver?=major|minor|patch,#{shortname('i', 's', 'u', 'd')},opts*"
                        task flag, [:semver] do |_, args|
                          opts = case (semver = args.semver)
                                 when 'major', 'minor', 'patch'
                                   args.extras
                                 else
                                   semver = nil
                                   args.to_a
                                 end
                          gem(flag, opts: opts, banner: true, filter: {
                            semver: semver,
                            update: has_value!(opts, 'u', 'update'),
                            interactive: has_value!(opts, 'i', 'interactive'),
                            select: has_value!(opts, 's', 'select'),
                            dryrun: has_value!(opts, 'd', 'dry-run')
                          })
                        end
                      when :build, :push, :exec, :update
                        format_desc(action, flag, 'opts*', after: case flag
                                                                  when :exec then 'command,args*'
                                                                  when :push then 'file/:'
                                                                  when :update then 'name*'
                                                                  end)
                        task flag do |_, args|
                          gem(flag, opts: args.to_a, banner: true)
                        end
                      when :install, :uninstall, :pristine
                        format_desc(action, flag, 'opts*', after: flag == :pristine ? 'name*|name?@version' : 'name*')
                        task flag do |_, args|
                          opts = param_guard(action, flag, args: args.to_a)
                          gem(flag, opts: opts, banner: true)
                        end
                      when :command
                        format_desc action, flag, 'command,opts*,args*'
                        task flag, [:command] do |_, args|
                          command = param_guard(action, flag, key: :command, args: args)
                          gem(command.to_sym, opts: args.extras, banner: true)
                        end
                      end
                    when 'bundle'
                      case flag
                      when :install, :update, :cache, :exec
                        format_desc(action, flag, 'opts*', after: case flag
                                                                  when :update then 'gems*'
                                                                  when :exec then 'command,args*|:'
                                                                  end)
                        task flag do |_, args|
                          bundle(flag, opts: args.to_a, banner: flag == :exec ? verbose? : true)
                        end
                      when :config
                        format_desc action, flag, 'list|set|get|unset?,opts*,args*'
                        task flag do |_, args|
                          bundle(flag, *args.to_a, banner: true)
                        end
                      when :reinstall
                        format_desc action, flag, 'f/orce?,opts*'
                        task flag do |_, args|
                          opts = args.to_a
                          opts << 'redownload' if has_value!(opts, 'f', 'force')
                          if (lock = basepath!('Gemfile.lock'))
                            config = basepath '.bundle', 'config'
                            if config.exist? && config.read.match?(/\bBUNDLE_FROZEN:\s+"true"/)
                              if opts.include?('redownload')
                                run(bundle_output('config unset frozen'), banner: false)
                              else
                                print_error('Gemfile.lock is frozen', subject: name, hint: flag)
                                lock = nil
                              end
                            end
                            lock&.delete
                          end
                          bundle(:install, opts: opts, banner: true)
                        end
                      when :command
                        format_desc action, flag, 'command,opts*,args*'
                        task flag, [:command] do |_, args|
                          command = param_guard(action, flag, key: :command, args: args)
                          bundle(command.to_sym, opts: args.extras, banner: true)
                        end
                      end
                    when 'ruby'
                      case flag
                      when :file
                        format_desc action, flag, 'path,opts*,args*'
                        task flag, [:rb] do |_, args|
                          opts = args.extras
                          args = Array(if (file = args.rb) && !file.include?('*')
                                         ENV['RUBY_ARGS']
                                       else
                                         a, b, c = choice_index('Select a file', Dir.glob(file || '*.rb', base: path),
                                                                values: (file ? [] : ['Options']).push('Arguments'),
                                                                series: true)
                                         if file
                                           file = a
                                           b
                                         else
                                           file = a
                                           opts.concat(OptionPartition.strip(b))
                                           c
                                         end
                                       end)
                          ruby(flag, *args, opts: opts, file: file)
                        end
                      when :script
                        format_desc action, flag, 'opts*'
                        task flag do |_, args|
                          command = ENV['RUBY_E'] || readline('Enter script', force: true, multiline: %w[## ;])
                          ruby(flag, opts: args.to_a, command: command)
                        end
                      when :version
                        format_desc action, flag
                        task flag do
                          ruby flag
                        end
                      end
                    end
                  end
                end
              end
            end
          end
        end

        def depend(*, sync: invoked_sync?('depend'), **)
          if @depend
            super
          elsif outdated?
            workspace.rev_clear(name, sync: sync)
            cmd = bundle_session 'install'
            option('binstubs') do |val|
              next if val == '0' || val == 'false'

              run(bundle_output('binstubs --all', case val
                                                  when '1', 'true'
                                                    nil
                                                  else
                                                    if val.start_with?('~')
                                                      val = File.join(Dir.home, val == '~' ? '.bundle' : val[1..-1])
                                                      if prod?
                                                        config_set('binstubs', shell_quote(val), global: true)
                                                        val = nil
                                                      end
                                                    else
                                                      val = basepath val
                                                    end
                                                    quote_option('path', val) if val
                                                  end), exception: false, banner: false, series: true)
            end
            if prod? && !config_get('without')
              if semgte?('3')
                config_set 'without', 'development'
              else
                cmd << '--without=development'
              end
            end
            option('jobs') { |n| cmd << "-j#{n}" if n.to_i > 0 }
            run_rb(sync: sync, from: :depend)
          end
        end

        def copy(from: gemlib, into: gemdir, override: false, **kwargs)
          return if @copy == false

          glob = kwargs[:include]
          pass = kwargs[:exclude]
          if @copy && !override
            return super unless @copy.is_a?(Hash)

            from = @copy[:from] if @copy.key?(:from)
            into = @copy[:into] if @copy.key?(:into)
            glob = @copy[:include] if @copy.key?(:include)
            pass = @copy[:exclude] if @copy.key?(:exclude)
          end
          return unless into

          on :first, :copy
          dest = Pathname.new(into).realpath
          print_item unless @output[0] || task_invoked?(/^copy(?::#{Ruby.ref}|$)/)
          glob = Array(glob || '**/*')
          Array(from).each_with_index do |val, i|
            a = basepath val
            b = dest + val
            c = glob[i] || glob.first
            log.info "cp #{a + c} #{b}"
            copy_dir(a, b, c, pass: pass, verbose: !silent?)
          rescue StandardError => e
            on_error e, :copy
          end
          on :last, :copy
        end

        def outdated(flag = nil, opts = [], sync: invoked_sync?('outdated', flag))
          cmd = bundle_output 'outdated'
          if flag
            se = has_value!(opts, 's', 'select')
            ia = has_value!(opts, 'i', 'interactive') && !se
            up = has_value!(opts, 'u', 'update')
            opts << 'only-explicit' if has_value!(opts, 'e', 'explicit')
            dryrun = has_value!(opts, 'd', 'dry-run')
            if !sync || stdin?
              se = false
              ia = false
            elsif se || ia || up
              items = []
            end
            OptionPartition.new(opts, bundleopts(:outdated), cmd << "--#{flag}", project: self)
                           .clear
          elsif (up = option('u', 'update', prefix: 'bundle'))
            flag = case up
                   when 'major', 'minor'
                     up.to_sym
                   else
                     :patch
                   end
            items = []
          end
          cmd << '--only-explicit' if option('only-explicit', prefix: 'bundle')
          dryrun ||= dryrun?(prefix: 'bundle')
          log.info cmd.to_s
          on :first, :outdated
          banner = format_banner cmd.to_s
          print_item banner if sync
          pwd_set(from: :outdated) do
            tc = theme[:current]
            start = 0
            found = 0
            major = 0
            col = 0
            buffer = []
            out = ->(val) { sync ? puts(val) : buffer << val }
            IO.popen(cmd.temp('--no-color')).readlines(chomp: true).each do |line|
              if start > 0
                n = line.size
                unless stdin?
                  line = line[0, col] if col > 0
                  data = line.scan(SEM_VER)
                  next unless (cur = data.shift) && (lat = data.shift)

                  semver cur
                  semver lat
                  type = semtype cur, lat
                  c = cur.join
                  l = lat.join
                  styles = []
                  ma = lambda do
                    styles = %i[green bold]
                    major += 1
                  end
                  mi = -> { styles[0] = type == 2 ? :yellow : :green }
                  if data.empty?
                    type == 1 ? ma.call : mi.call
                  else
                    data.each do |val|
                      break unless line =~ /(>=?|=|~>|!=|<=?) (#{Regexp.escape(val.join)})/

                      v = semver(val).join
                      case $1
                      when '>', '>='
                        type == 1 ? ma.call : mi.call
                      when '<', '<='
                        if c <= v
                          if type == 1
                            ma.call
                          else
                            styles[0] = :yellow
                          end
                        end
                      when '!='
                        if c == l
                          styles.clear
                        else
                          styles[1] = :bold
                        end
                      when '~>'
                        if c < v && cur[0] == val[0] && !semmajor?(cur, val)
                          styles[0] = :yellow
                        elsif semmajor?(val, lat)
                          styles[1] = :underline
                        else
                          styles[1] = :bold
                        end
                      end
                    end
                  end
                  name = line[/^\S+/, 0]
                  unless styles.empty?
                    case styles.first
                    when :green
                      sub_style!(line, **opt_style(theme[styles.last == :bold ? :major : :active], /^(\S+)(.+)$/))
                      found += 1
                    when :yellow
                      found += 1
                    end
                    sub_style!(line, **opt_style(tc, /^(.+)(#{Regexp.escape(c)})(.+)$/, 2)) if tc
                    sub_style!(line, **opt_style(colormap(styles), /^((?:\S+\s+){2})(#{Regexp.escape(l)})(.*)$/, 2))
                  end
                end
                s = '%2d. %s' % [start, line]
                if ia
                  unless confirm_semver(s.ljust(col + 4 + line.size - n), type)
                    start += 1
                    next
                  end
                elsif !se
                  out.call(s)
                end
                items&.push([line, name])
              elsif line.start_with?('Gem')
                unless stdin?
                  if ia
                    line.sub!(/\sGroups$/, '')
                    col = line.size
                  end
                  sub = [opt_style(theme[:header], /^(.+)(?<!\dm)(Gem|Latest)(.+)$/, 2)] * 2
                  out.call(print_footer(" #  #{line}", reverse: true, sub: sub))
                end
              else
                next
              end
              start += 1
            end
            unless sync
              print_item banner
              puts buffer
            end
            if found > 0
              unless Array(items).empty?
                gems = if se
                         choice('Select a package', items.map(&:first),
                                multiple: true, force: false, index: true, border: true).map! { |n| items[n.pred].last }
                       else
                         items.map(&:last)
                       end
                if dryrun
                  print_run bundle_output("update --#{flag}", *gems.quote!), false
                else
                  bundle(:update, *gems, opts: [flag.to_s])
                end
              end
              begin
                status = nil
                if gems
                  status = "#{gems.size} packages were updated"
                else
                  File.foreach(dependfile) do |line|
                    next unless line =~ /\b(?:source\s+(["'])((?~\1))\1|remote:\s+(\S+))/

                    status = ($2 || $3).chomp('/')
                    break
                  end
                end
              rescue StandardError => e
                log.debug e
              end
              puts print_footer(status || 'Updates are available', right: status.include?('/'))
            elsif start == 0 && banner
              puts 'No updates were found'
            end
          end
          on :last, :outdated
        end

        def ruby(*args, flag: nil, sync: true, banner: verbose?, with: nil, pass: PASS_RUBY[:ruby], **kwargs)
          flag = args.shift if !flag && args.first.is_a?(Symbol)
          if flag == :version
            pwd_set do
              out = []
              order = { 'rbenv' => -1, 'rvm' => -1, 'asdf' => -1, 'chruby' => -1 }
              ENV.fetch('PATH', '').split(':').each_with_index do |val, index|
                order.each_key do |key|
                  next unless val.match?(%r{[/.]#{key}/})

                  order[key] = index
                  break
                end
              end
              if @asdf
                [File.join(ENV.fetch('ASDF_DATA_DIR', '$HOME/.asdf'), "installs/#{@asdf.first}")]
              else
                [
                  "#{ENV.fetch('RBENV_ROOT', '$HOME/.rbenv')}/bin/rbenv",
                  '$HOME/.rvm/bin/rvm',
                  '/usr/bin/rbenv',
                  '/usr/local/rvm/bin/rvm',
                  '/usr/share/rvm/bin/rvm',
                  '/usr/local/share/chruby/chruby.sh'
                ].sort do |a, b|
                  c = -1
                  d = -1
                  order.each do |key, val|
                    pat = %r{/\.?#{key}}
                    c = val if a.match?(pat)
                    d = val if b.match?(pat)
                  end
                  if c == d
                    0
                  elsif c == -1
                    1
                  elsif d == -1
                    -1
                  else
                    c < d ? -1 : 1
                  end
                end
                .push('')
              end.each do |val|
                next unless val.empty? || File.exist?(val.sub('$HOME', Dir.home))

                trim = ->(s) { s[/^\D+\d+\.\d+(?:\.\S+)?/, 0].sub(/^([a-z]+)-/i, '\1 ') }
                ver = '.ruby-version'
                out << trim.call(case (cmd = File.basename(val))
                                 when 'rvm'
                                   `rvm current`[/^\S+/, 0]
                                 when 'rbenv'
                                   name = `rbenv version-name`
                                   name.match?(SEM_VER) ? "ruby #{name}" : name
                                 when 'chruby.sh'
                                   chruby = session_output 'source', val
                                   `#{chruby.with('ruby --version')}`
                                 else
                                   if @asdf
                                     cmd = 'asdf'
                                     ver = '.tool-versions'
                                     opt = [@asdf.first]
                                     opt.unshift('--no-header') unless @@asdf.version == 15
                                     `asdf current #{opt.join(' ')}`[/^\S+\s+\S+/, 0].sub(/\s+/, ' ')
                                   else
                                     ver = nil
                                     `ruby --version`
                                   end
                                 end)
                break if workspace.windows?

                unless val.empty?
                  out << trim.call(case cmd
                                   when 'chruby.sh'
                                     `#{chruby.with('chruby --version')}`.sub(':', '')
                                   when 'asdf'
                                     "asdf #{`asdf version`.delete_prefix('v')}"
                                   else
                                     `#{cmd} --version`
                                   end)
                end
                begin
                  out << ('which %s' % case cmd
                                       when 'rbenv'
                                         `rbenv which ruby`
                                       when 'chruby.sh'
                                         `#{chruby.with('which ruby')}`
                                       when 'asdf'
                                         `asdf which #{@asdf.first}`
                                       else
                                         `which ruby`
                                       end)
                rescue StandardError => e
                  log.debug e
                end
                if ver
                  path.ascend do |ent|
                    next unless (ent += ver).exist?

                    hint = File.read(ent).lines(chomp: true).reject(&:empty?).join(', ') rescue nil
                    out << message("found #{ent}", hint: hint)
                  end
                end
                break
              end
              out.map!(&:split)
              pad = out.map(&:first).map!(&:size).max
              print_item
              puts(out.map! { |line| '%*s %s' % [pad, line.first, line[1..-1].join(' ')] })
            end
            return
          end
          opts = session_opts(with, args: args, kwargs: kwargs, pass: pass)
          op = OptionPartition.new(opts, OPT_RUBY[:ruby], ruby_session, project: self, multiple: [/^-e/], args: true,
                                                                        stdin: true)
          if kwargs[:command]
            op << quote_option('e', kwargs[:command])
          elsif kwargs[:file]
            if op.include?('-')
              op.add_path(kwargs[:file])
            else
              op.unshift(basepath(kwargs[:file]))
            end
          end
          op.concat(args)
          if op.include?('-')
            op.exist?(add: true)
          else
            op.append_any { |val| OptionPartition.parse_arg!('e', val) }
            if op.arg?('e')
              op.clear
            else
              op.append(delim: true, escape: kwargs.fetch(:escape, false), quote: kwargs.fetch(:quote, false))
            end
          end
          from = if flag
                   :"ruby:#{flag}"
                 else
                   print_run(op, banner, **kwargs)
                   :ruby
                 end
          run_rb(sync: sync, banner: banner, exception: kwargs.fetch(:exception, exception), from: from)
        end

        def gem(flag, *args, sync: true, banner: verbose?, with: nil, pass: nil, **kwargs)
          flag = flag.to_sym
          if pass.nil?
            pass = case flag
                   when :install, :update
                     PASS_RUBY[:gem][:install]
                   when :dependency, :fetch, :info, :list, :outdated, :search, :specification
                     PASS_RUBY[:gem][:dependency]
                   else
                     PASS_RUBY[:gem].fetch(flag, [])
                   end
          end
          opts = session_opts(with, args: args, kwargs: kwargs, pass: pass)
          case flag
          when :build, :cert, :generate_index, :mirror, :outdated, :push, :server, :signin, :signout, :sources, :stale
            opts.concat(args)
          end
          op = OptionPartition.new(opts, gemopts(flag), gem_session(flag),
                                   project: self, no: OPT_GEM[:no][flag == :update ? :install : flag])
          from = :"gem:#{flag}"
          if flag == :outdated
            op.adjoin(gempwd, start: 0) if gempwd
            op.clear
            cmd = session_done op.target
            log.info cmd
            on :first, from
            banner = format_banner(cmd)
            print_item banner if sync
            major = [0, 0, 0]
            buffer = []
            filter = kwargs.fetch(:filter, {})
            semver = filter[:semver]
            update = if sync && filter[:select]
                       semver ||= 'major'
                       items = []
                       nil
                     elsif sync && filter[:interactive]
                       semver ||= 'major'
                       ia = true
                       []
                     elsif filter[:update]
                       semver ||= 'minor'
                       []
                     end
            out = ->(val) { sync ? puts(val) : buffer << val }
            pwd_set(pass: !gempwd.nil?, from: from) do
              rows = [[%w[Gem Current Latest], nil]]
              IO.popen(cmd).each do |line|
                if line =~ /^(\S+) \((\S+) < ([^)]+)\)$/
                  cur = semscan $2
                  lat = semscan $3
                  rows << [$~.to_a.drop(1), semtype(cur, lat)]
                else
                  out.call(line)
                end
              end
              if rows.size > 1
                pad = [rows.size.to_s.size.succ, 3].max
                d = 0
                e = 0
                f = 0
                j = 0
                queue = nil
                rows.each do |row|
                  a, b, c = row.first
                  d = a.size if a.size > d
                  e = b.size if b.size > e
                  f = c.size if c.size > f
                end
                rows.each_with_index do |row, i|
                  next if i == 0 && stdin?

                  a, b, c = row.first
                  type = row.last
                  if i == 0
                    line = '%-*s %-*s    %*s  %*s' % [pad, ' #', d, a, e, b, f, c]
                    s = ARG[:BORDER][1] * line.size
                    queue = if stdin?
                              [line, s]
                            else
                              2.times do
                                sub_style!(line, **opt_style(theme[:header], /^(.+)(?<!\dm)(#{a}|#{c})(.*)$/, 2))
                              end
                              [line, sub_style(s, borderstyle)]
                            end
                  else
                    g = a.ljust(d)
                    pat = [/^([^.]+\.)([^.]+\..+)$/, /^([^.]+\.[^.]+\.)(.+)$/]
                    pre = b.start_with?('0.')
                    latest = [theme[:latest]]
                    case type
                    when 1
                      case semver
                      when 'major'
                        update&.push(a)
                      when 'minor', 'patch'
                        next
                      end
                      unless stdin?
                        sub_style! g, theme[:major]
                        styles = %i[green bold]
                        pat = (pat.first if pre)
                        latest << :bold
                      end
                      major[0] += 1
                    when 2
                      case semver
                      when 'major', 'minor'
                        update&.push(a)
                      when 'patch'
                        next
                      end
                      unless stdin?
                        sub_style! g, theme[:active]
                        styles = %i[green]
                        pat = pre ? pat.last : pat.first
                      end
                      major[1] += 1
                    else
                      case semver
                      when 'major', 'minor', 'patch'
                        update&.push(a)
                      end
                      unless stdin?
                        styles = %i[yellow]
                        pat = pat.last
                      end
                      major[2] += 1
                    end
                    b = b.rjust(e)
                    h = c.rjust(f)
                    unless stdin?
                      sub_style!(b, **opt_style(colormap(styles), pat, 2))
                      sub_style!(h, **opt_style(latest.flatten.compact, pat, 2))
                    end
                    j += 1
                    if queue
                      out.call(queue)
                      queue = nil
                    end
                    s = ('%s    %s  %s' % [g, b, h]).yield_self do |val|
                      items&.push([val, a])
                      '%*s %s' % [pad, "#{j}.", val]
                    end
                    if ia
                      unless confirm_semver(s, type)
                        update.delete(a)
                        next
                      end
                    elsif !items
                      out.call(s)
                    end
                  end
                end
              end
            end
            unless sync
              print_item banner
              puts buffer
            end
            if major.sum == 0
              puts 'No updates were found'
            else
              if items
                update = choice('Select a package', items.map(&:first),
                                multiple: true, force: false, index: true, border: true).map! { |n| items[n.pred].last }
              end
              unless Array(update).empty?
                opts = ['f']
                option('document', prefix: 'gem', ignore: false) do |val|
                  opts << case val
                          when '0', 'false'
                            'no-document'
                          else
                            "document=#{val}"
                          end
                end
                option('user-install', prefix: 'gem', ignore: false) do |val|
                  opts << case val
                          when '0', 'false'
                            'no-user-install'
                          else
                            'user-install'
                          end
                end
                if filter[:dryrun]
                  print_run gem_output('update -f', *update.quote!), false
                else
                  gem(:update, *update, opts: opts)
                end
              end
              print_status(*major, from: :outdated)
            end
            on :last, from
            return
          end
          case flag
          when :check, :cleanup, :contents, :fetch, :info, :lock, :open, :owner, :pristine, :rdoc, :rebuild, :uninstall,
               :unpack, :update, :yank
            gems = true
            op.concat(args)
          when :dependency, :environment, :list, :search, :specification, :which
            op.concat(args)
          end
          ia = op.remove(':')
          op.each do |opt|
            if gems && !opt.start_with?('-') && !opt.match?(GEMNAME)
              op.errors << opt
            else
              op.found << opt
            end
          end
          op.swap do |a, b|
            return -1 if a.start_with?('-')

            b.start_with?('-') ? 1 : 0
          end
          case flag
          when :build
            if op.empty?
              raise_error Errno::ENOENT, 'gemspec', hint: project unless gemfile
              op.add_path(gemfile)
            else
              op.add_first(path: true)
                .clear(pass: false)
            end
          when :push
            if op.empty? || (n = op.index(':'))
              file = basepath(if !n && (spec = gemspec)
                                "#{spec.name}-#{spec.version}.gem"
                              else
                                choice_index 'Select a file', Dir.glob('*.gem', base: path)
                              end)
            else
              file = basepath(op.shift.yield_self { |val| val.include?('.') ? val : "#{val}.gem" })
              raise_error Errno::ENOENT, file, hint: flag unless file.exist?
              raise_error ArgumentError, "unrecognized args: #{op.join(', ')}", hint: flag unless op.empty?
            end
            op.add_path(file)
            return run_rb(from: from, interactive: ['Push', 'N', gemname]) unless with || !banner
          when :exec
            min = if op.arg?('g', 'gem')
                    1
                  elsif !op.empty?
                    op.add_first
                    0
                  elsif args.empty?
                    op << basic_option('gem', gemname)
                    1
                  else
                    0
                  end
            op.concat(args)
            if (args = command_args(op.extras, min: min, force: min == 1 && op.empty?))
              op.push(args)
            end
            op.append(quote: false)
          when :update
            if !op.arg?('n', 'bindir') && (bin = config_get('bin')) && Dir.exist?(bin)
              op << quote_option('bindir', bin)
            end
            if op.arg?('system')
              op.add_first(quote: false) { |val| val if val.match?(SEM_VER) }
            else
              op.append
            end
          when :install, :uninstall, :pristine
            if flag == :install
              post = if ia
                       op.concat(args)
                       readline('Enter command [args]', force: true)
                     elsif op.empty?
                       op.concat(args)
                       nil
                     elsif !args.empty?
                       args.join(' ')
                     end
            end
            raise_error ArgumentError, 'missing gem name', hint: flag if op.empty?
            if op.arg?('all')
              if flag == :pristine
                append_repeat 'skip', op.extras
                op.reset
              else
                op.clear
              end
            elsif (n = op.index { |val| val.match?(/(\A|[\w.-])@\d/) })
              name = op.remove_at(n)
              pre, ver = if (n = name.index('@')) == 0
                           [gemname, name[1..-1]]
                         else
                           [name[0, n], name[n.succ..-1]]
                         end
              op.adjoin(pre, quote_option('version', ver))
                .clear
            end
            if flag == :install
              op.append_any
            else
              op.append
            end
            op.delim << post if post
          when :check, :cleanup, :contents, :fetch, :list, :lock, :rdoc
            op.append
          when :dependency, :info, :search
            op.add_first(quote: true, expect: case flag
                                              when :dependency, :search then 'no pattern for gem name'
                                              else 'missing gem name'
                                              end)
              .clear
          when :environment
            unless op.empty?
              case (action = op.shift)
              when 'home', 'path', 'user_gemhome', 'version', 'remotesources', 'platform', 'credentials'
                op << action
                op.clear
              else
                raise_error ArgumentError, "unrecognized arg: #{action}", hint: flag
              end
            end
          when :open, :owner, :unpack, :yank
            op.add_first(gemname)
              .clear
          when :rebuild
            op.add_first(expect: 'missing gem name')
              .add_first(expect: 'missing gem version')
              .clear
          when :specification
            op.add_first(expect: 'missing gem name')
              .add_first
              .clear
          when :which
            op.splice(path: true) { |val| op.exist?(val) }
              .clear
          else
            op.clear
          end
          op.clear(errors: true) if gems
          print_run(op, banner, **kwargs)
          run_rb(sync: sync, banner: banner, exception: kwargs.fetch(:exception, exception), from: from)
        end

        def bundle(flag, *args, sync: true, banner: verbose?, with: nil, pass: nil, **kwargs)
          flag = flag.to_sym
          if pass.nil?
            pass = case flag
                   when :add, :outdated, :update
                     PASS_RUBY[:bundle][:update]
                   else
                     PASS_RUBY[:bundle].fetch(flag, [])
                   end
          end
          opts = session_opts(with, args: args, kwargs: kwargs, pass: pass)
          invalid = ->(a) { raise_error ArgumentError, "unrecognized args: #{a.join(', ')}", hint: flag }
          cmd = bundle_session(flag)
          case flag
          when :cache, :check, :clean, :init, :install, :lock, :pack, :package, :platform
            pre = true
            opts.concat(args)
          when :config
            if args.empty?
              pre = true
            else
              case (pre = args.shift)
              when 'list', 'get', 'set', 'unset'
                cmd << pre
              else
                args.unshift(pre)
              end
              opts.concat(args)
            end
          when :doctor
            case (pre = (val = args.shift) || opts.shift)
            when 'diagnose', 'ssl'
              cmd << pre
            else
              if val
                args.unshift(val)
              elsif pre
                opts.unshift(pre)
              end
              pre = true
            end
            opts.concat(args)
          when :plugin
            case (plu = args.shift || opts.shift)
            when 'install', 'uninstall', 'help', 'list'
              cmd << plu
            else
              invalid.call(plu)
            end
          end
          op = OptionPartition.new(opts, bundleopts(if pre == 'ssl'
                                                      :doctor_ssl
                                                    elsif plu
                                                      plu == 'install' ? :plugin : :"plugin_#{plu}"
                                                    else
                                                      flag
                                                    end),
                                   cmd,
                                   project: self, no: OPT_BUNDLE[:no][flag], args: flag == :exec || flag == :config)
          op.concat(args) unless pre
          output = false
          invalid = ->(a) { raise_error ArgumentError, "unrecognized args: #{a.join(', ')}", hint: flag }
          case flag
          when :config
            if pre == 'list'
              op.clear
            elsif !op.empty?
              a = op.dup
              b, c = op.slice!(0, 2)
              d = op.arg?('global', 'local')
              getname = -> { op << (b || readline('Enter name', force: true)) }
              case pre
              when 'get'
                getname.call
              when 'set'
                if d
                  op << b
                  b = c
                  c = op.shift
                end
                getname.call
                op << (c || readline('Enter value', force: true))
                output = true
              when 'unset'
                if d
                  op << b
                  b = c
                end
                getname.call
                output = true
              else
                if c && !op.arg?('parseable', 'no-parseable')
                  op.adjoin('set') << b
                  op.add_quote(c)
                  output = true
                  exit 1 unless confirm_basic('Confirm?', op, 'Y')
                elsif b
                  op.adjoin('get') << b
                  op.unshift(c) if c
                else
                  invalid.call(a)
                end
              end
              op.clear
            else
              val = readline('Enter arguments', force: false)
              op << (val.empty? ? 'list' : val)
              output = val.match?(/^(un)?set/)
            end
          when :plugin
            case plu
            when 'install', 'uninstall', 'help'
              op.append
            else
              op.clear
            end
          when :exec
            if op.empty? || (op.remove(':') && op.append(quote: false))
              op << readline('Enter arguments', force: true)
            else
              op.append(quote: false)
            end
          when :binstubs, :outdated, :remove, :update
            op.append(filter: GEMNAME)
              .clear(errors: true)
          when :add, :open, :show
            op.add_first(expect: 'missing gem name')
              .clear
          when :console, :gem
            op.add_first
              .clear
          else
            op.clear
          end
          print_run(op, banner, **kwargs)
          run(sync: sync, banner: banner, exception: kwargs.fetch(:exception, exception), from: :"bundle:#{flag}")
            .tap { |ret| success?(ret, banner, output) }
        end

        def rake(*args, sync: true, banner: verbose?, with: nil, pass: PASS_RUBY[:rake], **kwargs)
          opts = session_opts(with, args: args, kwargs: kwargs, pass: pass)
          op = OptionPartition.new(opts, OPT_RUBY[:rake], rake_session, project: self)
          op.adjoin(quote_option('f', rakefile)) if rakefile && !op.arg?('f', 'rakefile')
          op.concat(args)
          op.append(escape: true)
          print_run(op, banner, **kwargs)
          var = { 'BANNER' => '0' } unless banner
          run(op, var, sync: sync, banner: false, exception: kwargs.fetch(:exception, exception), from: :rake)
        end

        def irb(*args, banner: verbose?, with: nil, pass: PASS_RUBY[:irb], **kwargs)
          opts = session_opts(with, args: args, kwargs: kwargs, pass: pass)
          op = OptionPartition.new(opts, OPT_RUBY[:irb], session('irb'), project: self, first: [/\.rb$/])
          r = []
          r << 'bundler/setup' unless op.arg?('r')
          r << kwargs[:name] if kwargs[:name]
          r.each { |val| op.add_option('r', val, merge: true) }
          Array(kwargs.fetch(:path, gemlib)).each { |val| op << quote_option('I', val, merge: true) }
          op.concat(args)
          op.append(delim: true)
          print_run(op, banner, **kwargs)
          run(banner: false, exception: kwargs.fetch(:exception, exception), from: :irb)
        end

        def rbs(flag, *args, banner: verbose?, with: nil, pass: nil, **kwargs)
          case pass
          when NilClass
            pass = PASS_RUBY[:rbs]
          when Array
            pass += PASS_RUBY[:rbs]
          end
          opts = session_opts(with, args: args, kwargs: kwargs, pass: pass)
          cmd, opts = rbs_session(opts: opts)
          op = OptionPartition.new(opts, [], cmd << flag, project: self)
          case flag
          when :prototype
            sig = args.shift
            y = option('y', ignore: false)
            i = 1
            args.map! { |val| basepath(val).relative_path_from(path) }.each do |file|
              dir = basepath sig, file.dirname
              dir.mkpath unless dir.exist?
              base = file.basename.to_s
              rbs = dir + "#{base.stripext}.rbs"
              status = if rbs.exist?
                         case y
                         when '0', 'false'
                           'ignored'
                         else
                           next unless y || confirm_basic('Overwrite?', rbs, 'N')

                           'overwrite'
                         end
                       end
              unless status == 'ignored'
                ret = run(op.target.temp(File.extname(base) == '.rbi' ? 'rbi' : 'rb', file, '>', rbs), banner: false,
                                                                                                       series: true)
                if !ret
                  status = 'FAIL'
                elsif File.empty?(rbs)
                  status = 'empty'
                end
              end
              puts "#{i.to_s.rjust(2)}. #{rbs.relative_path_from(path)}".subhint(status)
              i += 1
            end
          else
            op.clear
              .append(*args)
            print_run(op, banner, **kwargs)
            run(banner: false, exception: kwargs.fetch(:exception, exception), from: :"rbs:#{flag}")
          end
        end

        def rubocop(*args, sync: true, banner: verbose?, with: nil, pass: PASS_RUBY[:rubocop], **kwargs)
          opts = session_opts(with, args: args, kwargs: kwargs, pass: pass)
          op = OptionPartition.new(opts, OPT_RUBY[:rubocop], session('rubocop'), project: self,
                                                                                 no: OPT_RUBY[:no][:rubocop])
          if @rubocopfile && !op.arg?('c', 'config') && !rootpath('.rubocop.yml', ascend: true).exist?
            op.add_path(@rubocopfile, option: 'c')
          end
          op.concat(args)
          op.each do |val|
            if basepath(val).file?
              op.found << val
            else
              op.errors << val
            end
          end
          op.swap
            .map! { |val| basepath(val).relative_path_from(path) }
          op.append(delim: true)
            .clear(errors: true)
          print_run(op, banner, **kwargs)
          run(sync: sync, banner: banner, exception: kwargs.fetch(:exception, exception), from: :rubocop)
        end

        def gemspec
          @gemspec = !gemfile.nil? && Gem::Specification.load(gemfile.to_s) rescue false if @gemspec.nil?
          @gemspec || nil
        end

        def gemname
          @gemname ||= ((spec = gemspec) ? spec.name : project)
        end

        def project
          return @project unless @project.frozen?

          @project = ((spec = gemspec) ? spec.name : @project).dup
        end

        def depend?
          @depend != false && (!@depend.nil? || outdated?)
        end

        def copy?
          return true if @copy.is_a?(Hash) ? copy[:into] : super
          return gemdir? if gemdir
          return false unless @autodetect

          set = lambda do |val, path|
            base = Pathname.new(path.strip)
            dir = base + gempath
            return false unless dir.writable? && base.join(gempath(val, 'specifications')).exist?

            log.warn "using version #{val}".subhint("given #{version}") if version && version != val
            self.version = val
            self.gemdir = dir
          end
          if version
            begin
              case @autodetect
              when 'rvm'
                pwd_set { `rvm info homes` }[/^\s+gem:\s+"(.+)"$/, 1]
              when 'rbenv'
                if pwd_set { `rbenv which ruby` } =~ %r{^(.+[\\/]versions[\\/](\d\.\d)\.[^\\/]+)[\\/]bin[\\/]ruby$}
                  File.join($1, 'lib/ruby/gems', "#{$2}.0")
                end
              when 'asdf'
                pwd_set { `asdf where ruby`.chomp }.yield_self do |val|
                  val =~ /(\d\.\d)\.[^.]+$/ && File.join(val, 'lib/ruby/gems', "#{$1}.0")
                end
              when /bundler?/
                pwd_set { `bundle env` }[/^\s+Gem Path\s+(.+)$/, 1].split(File::PATH_SEPARATOR).find do |val|
                  Dir.exist?(File.join(val, 'gems'))
                end
              else
                ENV['GEM_HOME'] || ENV['GEM_ROOT']
              end.tap do |val|
                return true if val && set.call(version, val)
              end
            rescue StandardError => e
              log.debug e
            end
            pwd_set(pass: !gempwd.nil?) do
              out = `#{gem_output(gempwd, 'list --local -d', gemname)}`
              next unless out =~ /#{Regexp.escape(gemname)}\s+\((.+)\)$/

              split_escape($1)
                .unshift(version)
                .uniq
                .each do |val|
                  next unless out =~ /(?:\(#{Regexp.escape(val)}[^)]*\)|Installed at):\s+(.+)$/

                  return gemdir? if set.call(val, $1)
                end
            end
            self.gemdir = Pathname.new(Gem.dir) + gempath
          else
            parse = lambda do |path|
              return false unless path

              ver = path[Regexp.new(['', 'gems', "#{gemname}-([^#{File::SEPARATOR}]+)", ''].join(File::SEPARATOR)), 1]
              if ver && (val = path[/\A(.+)#{Regexp.escape(gempath(ver))}/, 1])
                set.call(ver, val)
              end
            end
            if semgte?('2.6')
              target = RUBY_VERSION.start_with?('2.6') ? RubyVM : $LOAD_PATH
              parse.call(target.resolve_feature_path(gemname)&.last)
            end
            unless gemdir || pwd_set { parse.call(`#{bundle_output('show', gemname)}`) }
              raise_error Errno::ENOENT, 'gems home'
            end
          end
        rescue StandardError => e
          log.error e
          self.version = nil
          @gemdir = nil
          @autodetect = false
        else
          gemdir?
        end

        def outdated?
          dependtype > 0 && !task_pass?('outdated')
        end

        private

        def run_rb(**kwargs)
          run(banner: !@session&.include?('--quiet'), **kwargs)
        end

        def ruby_session(*cmd, **kwargs)
          session('ruby', *preopts, *cmd, **kwargs)
        end

        def gem_session(*cmd, **kwargs)
          session('gem', *cmd, *preopts, **kwargs)
        end

        def bundle_session(*cmd, **kwargs)
          session('bundle', *cmd, *preopts, **kwargs).tap { append_nocolor }
        end

        def rake_session(*cmd, **kwargs)
          session('rake', *preopts, *cmd, **kwargs)
        end

        def rbs_session(*cmd, opts: nil)
          return session('rbs', *cmd) unless opts

          op = OptionPartition.new(opts, OPT_RUBY[:rbs], project: self)
          [session('rbs', *op.to_a, *cmd), op.extras]
        end

        def gem_output(*cmd, **kwargs)
          session_output('gem', *cmd, **kwargs)
        end

        def ruby_output(*cmd, **kwargs)
          session_output('ruby', *cmd, **kwargs)
        end

        def bundle_output(*cmd, **kwargs)
          session_output('bundle', *cmd, **kwargs)
        end

        def rake_output(*cmd, **kwargs)
          session_output('rake', *cmd, **kwargs)
        end

        def config_get(key)
          out = pwd_set { `#{bundle_output('config get --parseable', key)}`.chomp }
          return unless out =~ /\A([^=]+)=(.*)\z/ && $1 == key

          case (out = $2)
          when 'true'
            true
          when '', '[]'
            nil
          else
            if out =~ /\A\[:(.+)\]\z/
              $1.split(', :').map { |val| ((val.delete_prefix!('"') && val.delete_suffix!('"')) || val).to_sym }
            else
              out || false
            end
          end
        end

        def config_set(key, *val, global: false)
          run(bundle_output('config', ('--global' if global), 'set', key, *val), banner: false, series: true)
        end

        def unpack_get(tag, ext)
          return super unless ext == 'gem'

          "https://rubygems.org/downloads/#{File.basename(tag, '.gem')}.gem"
        end

        def preopts
          verbose? ? ['--verbose'] : []
        end

        def variables
          (super + %i[autodetect]).freeze
        end

        def rakefile
          if @rakefile.nil?
            file = Rake::Application::DEFAULT_RAKEFILES.find { |val| exist?(val) }
            @rakefile = !file.nil? && basepath(file)
          end
          @rakefile || nil
        end

        def rakepwd
          return unless !pwd? && semgte?(Rake::VERSION, '13.0.4')

          quote_option 'C', path
        end

        def raketasks
          @raketasks ||= [].tap do |ret|
            opt = rakepwd
            pwd_set(pass: !opt.nil?) do
              IO.popen(rake_output(opt, '-AT').to_s).each do |line|
                next unless line =~ /^rake ((?:[^\[: ]+:?)+)(\[[^\]]+\])?/

                ret << [$1, $2]
              end
            end
          end
        end

        def bundleopts(*args)
          case args.first
          when :install, :update
            args << :install_a
          when :add, :plugin
            args << :common_git
          when :binstubs, :cache
            args << :common_all
          when :lock, :outdated
            args << :common_version
          end
          OPT_BUNDLE[:common] + args.flat_map { |name| OPT_BUNDLE.fetch(name, []) }
        end

        def gemopts(*args)
          case args.first
          when :install, :update
            args << :common_url << :common_domain << :install_a
          when :dependency, :info, :outdated, :search, :specification
            args << :common_url << :common_domain
          when :fetch, :list
            args << :common_url
          when :owner, :push, :yank
            args << :common_otp
          when :package, :pack
            args << :cache
          end
          case args.first
          when :info, :list, :search
            args << :common_all
          end
          OPT_GEM[:common] + args.flat_map { |name| OPT_GEM.fetch(name, []) }
        end

        def gempwd
          return unless !pwd? && semgte?(Gem::VERSION, '3.4.2')

          quote_option 'C', path
        end

        def gemfile
          if @gemfile.nil?
            @gemfile = [@project, name].map { |val| basepath("#{val}.gemspec") }
                                       .concat(path.glob('*.gemspec'))
                                       .find(&:exist?) || false
          end
          @gemfile || nil
        end

        def gemlib
          @gemlib ||= Set.new(['lib']).yield_self do |lib|
            if (spec = gemspec)
              lib.merge(spec.require_paths || [])
            end
            lib.select { |file| exist?(file) }
          end
        end

        def gempath(val = version, dir = 'gems')
          ret = File.join(dir, "#{gemname}-#{val}")
          ret += '.gemspec' if dir == 'specifications'
          ret
        end

        def gemdir?
          return false unless gemdir

          gemdir.exist? && !gemdir.empty? && gemdir.writable?
        end
      end

      Application.implement Ruby

      class Docker < Base
        COMPOSEFILE = %w[compose.yaml compose.yml docker-compose.yaml docker-compose.yml].freeze
        BAKEFILE = %w[docker-bake.json docker-bake.hcl docker-bake.override.json docker-bake.override.hcl].freeze
        DIR_DOCKER = (COMPOSEFILE + BAKEFILE + ['Dockerfile']).freeze
        OPT_DOCKER = {
          common: %w[tls tlsverify config=p c|context=b D|debug H|host=q l|log-level=b tlscacert=p tlscert=p
                     tlskey=p].freeze,
          buildx: {
            common: %w[builder=b D|debug],
            build: %w[add-host=q annotation=q attest=q build-arg=qq build-context=qq cache-from=q cache-to=q
                      cgroup-parent=b iidfile=p label=q network=b no-cache-filter=b o|output=q platform=q
                      q|quiet secret=qq shm-size=b ssh=qq t|tag=b target=b ulimit=q].freeze,
            bake: %w[print list=q set=q].freeze,
            shared: %w[check load no-cache pull push allow=q call=b? f|file=p metadata-file=p progress=b provenance=q
                       sbom=q].freeze
          }.freeze,
          compose: {
            common: %w[all-resources ansi|b compatibility dry-run env-file=p f|file=p parallel=n profile=b progress=b
                       project-directory=p p|project-name=e].freeze,
            build: %w[check no-cache print pull push with-dependencies q|quiet build-arg=qq builder=b m|memory=b
                      provenance=q sbom=q ssh=qq].freeze,
            create: %w[build force-recreate no-build no-recreate quiet-pull remove-orphans y|yes pull=b scale=i].freeze,
            exec: %w[d|detach privileged e|env=qq index=i T|no-TTY=b? user=e w|workdir=q].freeze,
            run: %w[build d|detach no-deps q|quiet quiet-build quiet-pull remove-orphans rm P|service-ports use-aliases
                    cap-add=b cap-drop=b entrypoint=q e|env=qq env-from-file=p i|interactive=b? l|label=q name=b
                    T|no-TTY=b? p|publish=q pull=b u|user=e v|volume=q w|workdir=q].freeze,
            up: %w[abort-on-container-exit abort-on-container-failure always-recreate-deps attach-dependencies build
                   d|detach force-recreate menu no-build no-color no-deps no-log-prefix no-recreate no-start quiet-build
                   quiet-pull remove-orphans V|renew-anon-volumes timestamps wait w|watch y|yes attach=b
                   exit-code-from=b no-attach=b pull=b scale=i t|timeout=i wait-timeout=i].freeze,
            down: %w[remove-orphans v|volumes rmi=b t|timeout=i].freeze
          }.freeze,
          container: {
            create: %w[init i|interactive no-healthcheck oom-kill-disable privileged P|publish-all q|quiet read-only
                       rm t|tty use-api-socket add-host=q annotation=q a|attach=b blkio-weight=i blkio-weight-device=i
                       cap-add=b cap-drop=b cgroup-parent=b cgroupns=b cidfile=p device=q device-cgroup-rule=q
                       device-read-bps=q device-read-iops=q device-write-bps=q device-write-iops=q
                       dns=q dns-option=q dns-search=q domainname=b entrypoint=q e|env=qq env-file=p expose=q gpus=q
                       group-add=b health-cmd=q health-interval=b health-retries=i health-start-interval=q
                       health-start-period=q health-timeout=q hostname=q io-maxbandwidth=b io-maxiops=b ip=b ip6=q ipc=b
                       isolation=b l|label=q label-file=q link=b link-local-ip=q log-driver=b log-opt=q mac-address=q
                       m|memory=b memory-reservation=b memory-swap=n memory-swappiness=n mount=qq name=b network=b
                       network-alias=b oom-score-adj=b pid=b pids-limit=n platform=q p|publish=q pull=b restart=b
                       runtime=b security-opt=q shm-size=b stop-signal=b stop-timeout=i storage-opt=q sysctl=q tmpfs=q
                       ulimit=q u|user=b userns=b uts=b v|volume=q volume-driver=b volumes-from=b w|workdir=q].freeze,
            run: %w[d|detach detach-keys=q sig-proxy=b?].freeze,
            update: %w[blkio-weight=i cpu-period=i cpu-quota=i cpu-rt-period=i cpu-rt-runtime=i c|cpu-shares=i cpus=f
                       cpuset-cpus=b cpuset-mems=b m|memory=b memory-reservation=b memory-swap=n pids-limit=n
                       restart=q].freeze,
            exec: %w[d|detach i|interactive privileged t|tty detach-keys=q e|env=qq env-file=p u|user=e
                     w|workdir=q].freeze,
            commit: %w[no-pause a|author=q c|change=q m|message=q pause=b?].freeze,
            inspect: %w[s|size f|format=q type=b].freeze,
            start: %w[a|attach i|interactive detach-keys=q].freeze,
            stop: %w[s|signal=b t|timeout=i].freeze,
            restart: %w[s|signal=b t|timeout=i].freeze,
            kill: %w[s|signal=b].freeze,
            stats: %w[a|all no-stream no-trunc format|q].freeze
          }.freeze,
          image: {
            ls: %w[a|all digests no-trunc q|quiet tree f|filter=q format=q].freeze,
            push: %w[a|all-tags platform=q q|quiet].freeze,
            rm: %w[f|force no-prune platform=q].freeze,
            save: %w[o|output=p platform=q].freeze
          }.freeze,
          network: {
            connect: %w[alias=b driver-opt=q gw-priority=n ip=b ip6=q link=b link-local-ip=q].freeze,
            disconnect: %w[f|force].freeze
          }.freeze
        }.freeze
        VAL_DOCKER = {
          run: {
            common: %w[source src destination dst target readonly ro].freeze,
            bind: %w[bind-propagation].freeze,
            volume: %w[volume-subpath volume-nocopy volume-opt].freeze,
            tmpfs: %w[tmpfs-size tmpfs-mode].freeze,
            image: %w[image-path].freeze
          }.freeze,
          ls: {
            compose: %w[Name Image Command Service RunningFor Status Ports CreatedAt ExitCode Health ID Labels
                        LocalVolumes Mounts Names Networks Project Publishers Size State].freeze,
            container: %w[ID Image Command RunningFor Status Ports Names CreatedAt Labels LocalVolumes Mounts Networks
                          Platform Size State].freeze,
            image: %w[Repository Tag ID Containers CreatedSince Size CreatedAt Digest SharedSize UniqueSize
                      VirtualSize].freeze,
            network: %w[ID Name Driver Scope CreatedAt IPv4 IPv6 Internal Labels].freeze
          }.freeze
        }.freeze
        private_constant :COMPOSEFILE, :BAKEFILE, :OPT_DOCKER, :VAL_DOCKER

        class << self
          def tasks
            [].freeze
          end

          def config?(val)
            return false unless (val = as_path(val))

            DIR_DOCKER.any? { |file| val.join(file).exist? }
          end
        end

        subtasks({
          'build' => %i[tag context].freeze,
          'compose' => %i[build create run exec up down service].freeze,
          'bake' => %i[build check].freeze,
          'image' => %i[ls rm push tag save].freeze,
          'container' => %i[run create exec update commit inspect diff start stop restart pause unpause top stats kill
                            rm].freeze,
          'network' => %i[connect disconnect].freeze,
          'ls' => nil
        })

        attr_reader :context
        attr_accessor :tag

        def initialize(*, file: nil, context: nil, tag: nil, secrets: nil, mounts: [], registry: nil, **kwargs)
          super
          return unless dockerfile(file).exist?

          @context = context
          self.tag = tag || tagname("#{@project}:#{@version || 'latest'}")
          @mounts = mounts
          @secrets = secrets
          @registry = tagjoin registry, kwargs[:username]
          initialize_ref Docker.ref
          initialize_logger(**kwargs)
          initialize_env(**kwargs)
          @output[4] = merge_opts(kwargs[:args], @output[4]) if kwargs[:args]
        end

        def ref
          Docker.ref
        end

        def populate(*, **)
          super
          return unless ref?(Docker.ref) || @only

          namespace name do
            Docker.subtasks do |action, flags|
              next if task_pass?(action)

              if flags.nil?
                case action
                when 'ls'
                  format_desc(action, nil, VAL_DOCKER[:ls].keys, after: 'a/ll?,s/tandard?,range*', arg: nil)
                  task action, [:command] do |_, args|
                    command = param_guard(action, 'command', args: args, key: :command)
                    args = args.extras
                    cmd = docker_output command, case command
                                                 when 'image', 'container', 'network'
                                                   'ls'
                                                 when 'compose'
                                                   'ps'
                                                 else
                                                   raise_error ArgumentError, 'unrecognized command', hint: command
                                                 end
                    cmd << '-a' if has_value!(args, 'a', 'all') && command != 'network'
                    data = VAL_DOCKER[:ls][command.to_sym]
                    cols = if has_value!(args, 's', 'standard')
                             data.first(data.index('CreatedAt'))
                           else
                             [].tap do |out|
                               args.each do |val|
                                 if val =~ /^(\d+)$/
                                   out << data[$1.to_i.pred]
                                 elsif val =~ /^(\d+)(-|\.{2,3})(\d+)$/
                                   j = $1.to_i.pred
                                   k = $3.to_i - ($2 == '..' ? 2 : 1)
                                   out.concat(data[j..k]) if k > j
                                 end
                               end
                               next unless out.empty?

                               out.replace(choice_index('Select a column', data, multiple: true, attempts: 1))
                             end
                           end
                    cmd << quote_option('format', "table #{cols.map! { |val| "{{.#{val}}}" }.join("\t")}")
                    run(cmd, banner: false, from: :ls)
                  end
                end
              else
                namespace action do
                  flags.each do |flag|
                    case action
                    when 'build'
                      format_desc(action, flag, 'opts*', before: flag == :tag ? 'name' : 'dir')
                      task flag, [flag] do |_, args|
                        param = param_guard(action, flag, args: args, key: flag)
                        buildx(:build, args.extras, "#{flag}": param)
                      end
                    when 'bake'
                      break unless bake?

                      case flag
                      when :build
                        format_desc action, flag, 'opts*,target*,context|:'
                        task flag do |_, args|
                          args = args.to_a
                          if args.first == ':'
                            choice_command :bake
                          else
                            buildx :bake, args
                          end
                        end
                      when :check
                        format_desc action, flag, 'target'
                        task flag, [:target] do |_, args|
                          target = param_guard(action, flag, args: args, key: :target)
                          buildx :bake, ['allow=fs.read=*', 'call=check', target]
                        end
                      end
                    when 'compose'
                      break unless compose?

                      case flag
                      when :exec, :run
                        format_desc action, flag, "service/:,command#{'?' unless flag == :exec}/::,args*,opts*"
                        task flag, [:service] do |_, args|
                          service = param_guard(action, flag, args: args, key: :service)
                          compose!(flag, args.extras, service: service)
                        end
                      when :service
                        cmds = %w[down kill pause restart rm start stop top unpause watch].freeze
                        format_desc(action, flag, cmds, arg: nil, after: 'name+|:')
                        task flag, [:command] do |_, args|
                          command = param_guard(action, flag, args: args, key: :command)
                          raise_error ArgumentError, 'unrecognized command', hint: command unless cmds.include?(command)
                          service = args.extras
                          if service.first == ':'
                            choice_command flag, command
                          else
                            compose!(flag, [command], service: service.empty? || service)
                          end
                        end
                      else
                        format_desc action, flag, 'opts*,service*|:'
                        task flag do |_, args|
                          compose!(flag, args.to_a, multiple: true)
                        end
                      end
                    when 'container'
                      case flag
                      when :exec, :commit
                        format_desc(action, flag, flag == :exec ? 'id/name,opts*,args+|:' : 'id/name,tag?,opts*')
                        task flag, [:id] do |_, args|
                          if flag == :exec && !args.id
                            choice_command flag
                          else
                            id = param_guard(action, flag, args: args, key: :id)
                            container(flag, args.extras, id: id)
                          end
                        end
                      when :run, :create
                        format_desc action, flag, 'image,opts*,args*|:'
                        task flag, [:image] do |_, args|
                          if args.image
                            container(flag, args.extras, id: args.image)
                          else
                            choice_command flag
                          end
                        end
                      else
                        format_desc action, flag, "opts*,id/name#{flag == :update ? '+' : '*'}"
                        task flag do |_, args|
                          container flag, args.to_a
                        end
                      end
                    when 'image'
                      case flag
                      when :push
                        format_desc action, flag, 'tag,registry/username?,opts*'
                        task flag, [:tag] do |_, args|
                          id = param_guard(action, flag, args: args, key: :tag)
                          image(flag, args.extras, id: id)
                        end
                      else
                        format_desc(action, flag, case flag
                                                  when :rm, :save then 'id,opts*'
                                                  when :tag then 'version'
                                                  else 'opts*,args*'
                                                  end, before: 'pattern?')
                        task flag do |_, args|
                          args = args.to_a
                          n = args.size
                          if (n > 1 || (flag == :ls && n > 0)) && OptionPartition.pattern?(args.first)
                            filter = args.shift
                          end
                          if !args.empty? || flag == :ls
                            image(flag, args, filter: filter)
                          else
                            choice_command flag
                          end
                        end
                      end
                    when 'network'
                      format_desc action, flag, 'target,opts*'
                      task flag, [:target] do |_, args|
                        if args.target
                          network(flag, args.extras, target: args.target)
                        else
                          choice_command flag
                        end
                      end
                    end
                  end
                end
              end
            end
          end
        end

        def clean(*, sync: invoked_sync?('clean'), **)
          if runnable?(@clean)
            super
          elsif sync || option('y', prefix: 'docker')
            image :rm
          end
        end

        def compose(opts, flags = nil, script: false, args: nil, from: :run, **)
          return opts unless script

          if from == :run
            if bake?(n = filetype)
              ret = docker_session 'buildx bake'
              append_file n
              from = :bake
            elsif compose?(n)
              ret = docker_session 'compose build'
              append_file n
              from = :compose
            else
              ret = docker_session 'build'
            end
          else
            ret = docker_session from
          end
          case opts
          when String
            ret << opts
          when Hash
            ret.merge(append_hash(opts, target: [], build: true))
          when Enumerable
            ret.merge(opts.to_a)
          end
          [args, flags].each_with_index do |item, i|
            next unless item && (data = append_any(item, target: []))

            ret.merge(data.map! { |arg| i == 0 ? fill_option(arg) : quote_option('build-arg', arg) })
          end
          case from
          when :run
            case @secrets
            when String
              ret << quote_option('secret', @secrets, double: true)
            when Hash
              append = lambda do |type|
                Array(@secrets[type]).each { |arg| ret << quote_option('secret', "type=#{type},#{arg}", double: true) }
              end
              append.call(:file)
              append.call(:env)
            else
              Array(@secrets).each { |arg| ret << quote_option('secret', arg) }
            end
            if (val = option('tag', ignore: false))
              append_tag val
            elsif !session_arg?('t', 'tag')
              append_tag tag
            end
            append_context
          when :bake, :compose
            option(from == :bake ? 'target' : 'service', ignore: false) { |val| ret.merge(split_escape(val).quote!) }
          end
          ret
        end

        def buildx(flag, opts = [], tag: nil, context: nil)
          cmd, opts = docker_session('buildx', opts: opts)
          op = OPT_DOCKER[:buildx].yield_self do |data|
            OptionPartition.new(opts, data[:common], cmd, project: self)
                           .append(flag, quote: false)
                           .parse(data[flag == :bake ? :bake : :build] + data[:shared])
          end
          case flag
          when :build, :context
            append_tag(tag || option('tag', ignore: false) || self.tag)
            append_context context
          when :bake
            unless op.empty?
              args = op.dup
              op.reset
              if Dir.exist?(args.last)
                if projectpath?(val = args.pop)
                  context = val
                else
                  op.push(val)
                end
              end
              op.append(args, escape: true, strip: /^:/)
              contextdir context if context
            end
          end
          op.clear(pass: false)
          run(from: :"buildx:#{flag}")
        end

        def compose!(flag, opts = [], service: nil, multiple: false)
          from = :"compose:#{flag}"
          if flag == :service
            command = opts.first
            if service == true
              cmd, status = filter_ps command, from
              lines = IO.popen(cmd.temp('--services')).map(&:strip).reject(&:empty?)
              return list_empty(hint: status) if lines.empty?

              service = choice_index('Choose a service', lines, multiple: true, attempts: 1)
            end
            docker_session('compose', command, '--', *service)
          else
            cmd, opts = docker_session('compose', opts: opts)
            op = OptionPartition.new(opts, OPT_DOCKER[:compose][:common], cmd, project: self)
            append_file filetype unless op.arg?('f', 'file')
            op << flag
            op.parse(OPT_DOCKER[:compose].fetch(flag, []))
            if op.remove(':') || service == ':'
              keys = Set.new
              read_composefile('services', target: op.values_of('f', 'file')) { |data| keys.merge(data.keys) }
              service = unless keys.empty?
                          choice_index('Add services', keys, multiple: multiple, force: !multiple,
                                                             attempts: multiple ? 1 : 3)
                        end
            end
            if multiple
              op.concat(service) if service
              op.append(delim: true, escape: true, strip: /^:/)
            else
              raise_error ArgumentError, 'no service was selected', hint: flag unless service
              append_command(flag, service, op.extras, prompt: '::')
            end
          end
          run(from: from)
        end

        def container(flag, opts = [], id: nil)
          cmd, opts = docker_session('container', flag, opts: opts)
          rc = flag == :run || flag == :create
          op = OPT_DOCKER[:container].yield_self do |data|
            list = data.fetch(flag, [])
            list += data[:create] if flag == :run
            list += data[:update] if rc
            OptionPartition.new(opts, list, cmd, project: self, args: rc || flag == :exec)
          end
          from = :"container:#{flag}"
          case flag
          when :run, :create, :exec
            if rc && !op.arg?('mount')
              all = collect_hash VAL_DOCKER[:run]
              delim = Regexp.new(",\\s*(?=#{all.join('|')})")
              Array(@mounts).each do |val|
                args = []
                type = nil
                val.split(delim).each do |opt|
                  k, v, q = split_option opt
                  if k == 'type'
                    case v
                    when 'bind', 'volume', 'image', 'tmpfs'
                      type = v
                    else
                      raise_error TypeError, "unknown: #{v}", hint: flag
                    end
                  elsif all.include?(k)
                    unless type
                      VAL_DOCKER[:run].each_pair do |key, a|
                        next unless a.include?(k)

                        type = key.to_s unless key == :common
                        break
                      end
                    end
                    case k
                    when 'readonly', 'ro'
                      args << k
                      next
                    when 'source', 'src', 'destination', 'dst', 'target', 'volume-subpath', 'image-path'
                      v = basepath v
                      v = shell_quote(v, option: false, force: false) if q == ''
                    end
                    args << "#{k}=#{q + v + q}"
                  elsif !silent?
                    log_message('unrecognized option', subject: from, hint: k)
                  end
                end
                raise_error TypeError, 'none specified', hint: flag unless type
                cmd << "--mount type=#{type},#{args.join(',')}"
              end
            end
            append_command(flag, id || tagmain, op.extras)
          when :update
            raise_error ArgumentError, 'missing container', hint: flag if op.empty?
            op.append(escape: true, strip: /^:/)
          when :commit
            latest = op.shift || tagmain
            cmd << id << latest
            raise_error ArgumentError, "unrecognized args: #{op.join(', ')}", hint: flag unless op.empty?
            return unless confirm_command(cmd.to_s, title: from, target: id, as: latest)

            registry = option('registry') || @registry
            run(from: from, exception: registry.nil? ? exception : true)
            return unless registry

            opts = []
            append_option('platform', target: opts, equals: true)
            opts << case option('disable-content-trust', ignore: false)
                    when 'false', '0'
                      '--disable-content-trust=false'
                    else
                      '--disable-content-trust'
                    end
            opts << '--quiet' if silent?
            return image(:push, opts, id: latest, registry: registry)
          else
            if op.empty?
              ps, status, no = filter_ps flag, from
              cmd << '--no-stream' if flag == :stats
              list_image(flag, ps, no: no, hint: status, from: from) { |img| run(cmd.temp(img), from: from) }
              return
            end
            op.append(escape: true, strip: /^:/)
          end
          run(from: from)
        end

        def image(flag, opts = [], sync: true, id: nil, registry: nil, filter: nil)
          cmd, opts = docker_session('image', flag, opts: opts)
          op = OptionPartition.new(opts, OPT_DOCKER[:image].fetch(flag, []), cmd, project: self)
          exception = self.exception
          banner = true
          from = :"image:#{flag}"
          case flag
          when :ls
            if opts.size == op.size
              index = 0
              name = nil
              opts.reverse_each do |val|
                next unless (arg = OptionPartition.parse_arg!('name', val))

                name = arg[1]
                opts.delete(val)
                break
              end
              list_image(:run, filter: filter, from: from) do |val|
                container(:run, if name
                                  opts.dup << "name=#{index == 0 ? name : "#{name}-#{index}"}"
                                else
                                  opts
                                end, id: val)
                index += 1
              end
              return
            end
            op.clear
          when :rm
            unless id
              if op.empty?
                list_image(:rm, filter: filter, from: from) { |val| image(:rm, opts, sync: sync, id: val) }
              else
                op.each { |val| run(cmd.temp(val), sync: sync, from: from) }
              end
              return
            end
            op << id
            if option('y')
              exception = false
              banner = false
            end
          when :tag, :save
            found = false
            list_image(flag, filter: filter, from: from) do |val|
              op << val
              found = true
              if flag == :tag
                op << tagname("#{project}:#{op.first}")
                break
              end
            end
            raise_error ArgumentError, 'target not specified', hint: flag unless found
          when :push
            id ||= option('tag', ignore: false) || tagmain
            registry ||= op.shift || option('registry') || @registry
            unless id && op.empty?
              if id
                raise_error ArgumentError, "unrecognized args: #{op.join(', ')}", hint: flag
              else
                raise_error 'no id/tag', hint: flag
              end
            end
            raise_error ArgumentError, 'username/registry not specified', hint: flag unless registry
            registry.chomp!('/')
            uri = shell_quote "#{registry}/#{id}"
            op << uri
            img = docker_output 'image', 'tag', id, uri
            return unless confirm_command(img.to_s, cmd.to_s, target: id, as: registry, title: from)

            cmd = img
            sync = false
            exception = true
            banner = false
          end
          run(cmd, sync: sync, exception: exception, banner: banner, from: from).tap do |ret|
            success?(ret, flag == :tag || flag == :save)
          end
        end

        def network(flag, opts = [], target: nil)
          cmd, opts = docker_session('network', flag, opts: opts)
          OptionPartition.new(opts, OPT_DOCKER[:network].fetch(flag, []), cmd, project: self)
                         .clear
          from = :"network:#{flag}"
          list_image(flag, docker_output('ps -a'), from: from) do |img|
            success?(run(cmd.temp(target, img), from: from))
          end
        end

        def build?
          @output[0] != false && dockerfile.exist?
        end

        def clean?
          super || dockerfile.exist?
        end

        def compose?(file = dockerfile)
          return file == 3 || file == 4 if file.is_a?(Numeric)

          COMPOSEFILE.include?(File.basename(file))
        end

        def bake?(file = dockerfile)
          return file == 1 || file == 2 if file.is_a?(Numeric)

          BAKEFILE.include?(File.basename(file))
        end

        def dockerfile(val = nil)
          if val
            @file = if val.is_a?(Array)
                      val = val.select { |file| exist?(file) }
                      val.size > 1 ? val : val.first
                    elsif val == true
                      DIR_DOCKER.find { |file| exist?(file) }
                    elsif val != 'Dockerfile'
                      val
                    end
          end
          basepath((@file.is_a?(Array) ? @file.first : @file) || 'Dockerfile')
        end

        private

        def read_composefile(*keys, target: nil)
          require 'yaml'
          target = ENV['COMPOSE_FILE']&.split(workspace.windows? ? ';' : ':') unless target && !target.empty?
          Array(target || dockerfile).each do |val|
            doc = YAML.load_file(basepath(val))
            if keys.empty?
              yield doc
            elsif (data = doc.dig(*keys))
              yield data
            end
          rescue StandardError => e
            log.debug e
          end
        end

        def docker_session(*cmd, opts: nil)
          return session('docker', *cmd) unless opts

          op = OptionPartition.new(opts, OPT_DOCKER[:common], project: self)
          [session('docker', *op.to_a, *cmd), op.extras]
        end

        def docker_output(*cmd, **kwargs)
          session('docker', *cmd, main: false, options: false, **kwargs)
        end

        def append_command(flag, val, list, target: @session, prompt: ':')
          if list.delete(prompt)
            list << readline('Enter command [args]', force: flag == :exec)
          else
            env('DOCKER_ARGS') { |args| list << args }
          end
          case flag
          when :run
            unless session_arg?('name', target: target)
              target << basic_option('name', dnsname("#{name}_%s" % rand_s(6)))
            end
          when :exec
            raise_error ArgumentError, 'nothing to execute', hint: flag if list.empty?
          end
          target << val << list.shift
          target << list.join(' && ') unless list.empty?
        end

        def append_file(type, target: @session, index: 2)
          return if !@file || (ENV['COMPOSE_FILE'] && compose?(type))

          unless @file.is_a?(Array)
            case type
            when 2, 4
              return
            when 3
              return unless COMPOSEFILE.select { |val| basepath!(val) }.size > 1
            end
          end
          files = Array(@file).map { |val| quote_option('file', basepath(val)) }
          if target.is_a?(Set)
            opts = target.to_a.insert(index, *files)
            target.clear
                  .merge(opts)
          else
            target.insert(index, *files)
          end
        end

        def append_context(ctx = nil, target: @session)
          if @file.is_a?(String) && !session_arg?('f', 'file', target: target) && !bake? && !compose?
            target << quote_option('file', dockerfile)
          end
          target << contextdir(ctx || context)
        end

        def append_tag(val, target: @session)
          ver = option('version', target: target, ignore: false)
          case val
          when String
            split_escape val
          else
            Array(val)
          end.each do |s|
            s = "#{s}:#{ver}" if ver && (!s.include?(':') || s.delete_suffix!(':latest'))
            target << basic_option('tag', tagname(s))
          end
          target
        end

        def filter_ps(flag, from = :'container:ps')
          no = false
          status = case flag.to_sym
                   when :start
                     %w[created exited]
                   when :stop, :pause
                     no = true
                     %w[running restarting]
                   when :restart
                     no = true
                     %w[running paused exited]
                   when :unpause
                     %w[paused]
                   when :top, :stats, :watch
                     %w[running]
                   when :kill
                     no = true
                     %w[running paused restarting]
                   when :rm
                     no = true
                     %w[created exited dead]
                   else
                     []
                   end
          cmd = docker_output("#{from.to_s.split(':').first} ps -a",
                              *status.map { |s| quote_option('filter', "status=#{s}") })
          [cmd, status, no]
        end

        def list_image(flag, cmd = docker_output('image ls -a'), filter: nil, hint: nil, no: true, from: nil)
          pwd_set(from: from) do
            index = 1
            all = option('all', prefix: 'docker')
            y = from == :'image:rm' && option('y', prefix: 'docker')
            filter = env('DOCKER_FILTER', filter).to_s
            pat = if OptionPartition.pattern?(filter)
                    Regexp.new(filter)
                  elsif filter.match?(/[:_-]$/)
                    /\b#{Regexp.escape(filter)}/
                  else
                    filter = filter.empty? ? '(?:[:_-]|$)' : "[:_-]#{filter}"
                    /\b(?:#{dnsname(name)}|#{tagname(project)}|#{tagmain.split(':', 2).first})#{filter}/
                  end
            IO.popen(cmd.temp('--format=json')).each do |line|
              data = JSON.parse(line)
              id = data['ID']
              rt = [data['Repository'], data['Tag']].reject { |val| val.to_s.empty? || val == '<none>' }.join(':')
              rt = nil if rt.empty?
              aa = data['Names'] || (if rt && data['Repository']
                                       dd = true
                                       data['Repository']
                                     else
                                       id
                                     end)
              ee = data['Image'] || rt || aa
              next unless all || ee.match?(pat) || aa.match?(pat)

              unless y
                bb = index.to_s
                cc = bb.size.succ
                a = sub_style ee, theme[:inline]
                b = "Execute #{sub_style(flag, theme[:active])} on #{a.subhint(ee == id ? nil : id)}"
                e = time_format(time_since(data['CreatedAt']), pass: ['ms'])
                f = sub_style ARG[:BORDER][0], theme[:inline]
                g = ' ' * cc.succ
                h = "#{sub_style(bb.rjust(cc), theme[:current])} #{f} "
                puts unless index == 1
                puts (h + sub_style(aa, theme[:subject])).subhint("created #{e} ago")
                cols = %w[Tag Status Ports]
                cols << case flag
                        when :connect, :disconnect
                          'Networks'
                        else
                          'Size'
                        end
                cols.each do |key|
                  next if (key == 'Tag' && !dd) || (key == 'Size' && data[key] == '0B')

                  puts "#{g + f} #{key}: #{Array(data[key]).join(', ')}" unless data[key].to_s.empty?
                end
                w = 9 + flag.to_s.size + 4 + ee.size
                puts g + sub_style(ARG[:BORDER][6] + (ARG[:BORDER][1] * w), theme[:inline])
                index += 1
                next unless confirm("#{h + b}?", no ? 'N' : 'Y')

                puts if printfirst?
              end
              yield id
            end
            list_empty(hint: hint || from) if index == 1 && !y
          end
        rescue StandardError => e
          on_error e, from
        end

        def list_empty(subject: name, hint: nil, **kwargs)
          hint = "status: #{hint.join(', ')}" if hint.is_a?(Array)
          puts log_message('none detected', subject: subject, hint: hint, **kwargs)
        end

        def confirm_command(*args, title: nil, target: nil, as: nil)
          return false unless title && target

          puts unless printfirst?
          t = title.to_s.split(':')
          emphasize(args, title: message(t.first.upcase, *t.drop(1)), border: borderstyle, sub: [
            opt_style(theme[:header], /\A(\w+(?: => \w+)+)(.*)\z/),
            opt_style(theme[:caution], /\A(.+)\z/)
          ])
          printsucc
          s = t.last.capitalize
          confirm "#{s} #{sub_style(target, theme[:subject])}#{" as #{sub_style(as, theme[:inline])}" if as}?", 'N'
        end

        def choice_command(flag, *action)
          msg, cmd = case flag
                     when :exec
                       ['Choose a container', 'ps -a']
                     when :bake
                       ['Choose a target', 'buildx bake --list=type=targets']
                     when :connect, :disconnect
                       ['Choose a network', 'network ls']
                     when :service
                       ['Choose a service',
                        'compose ps -a ' \
                        "--format='table {{.Service}}\t{{.Name}}\t{{.Image}}\t{{.Command}}\t{{.Status}}\t{{.Ports}}'"]
                     else
                       ['Choose an image',
                        'images -a ' \
                        "--format='table {{.ID}}\t{{.Repository}}\t{{.Tag}}\t{{.CreatedSince}}\t{{.Size}}'"]
                     end
          lines = `#{docker_output(cmd)}`.lines
          if lines.size <= 1
            puts log_message('none found', subject: name, hint: "docker #{cmd.split(' ', 3)[0, 2].join(' ')}")
            return
          end
          puts " #  #{lines.shift}"
          multiple = false
          ctx = flag.to_s
          case flag
          when :run, :exec
            values = [['Options', flag == :run], ['Arguments', flag == :exec]]
          when :rm, :bake
            values = ['Options']
            multiple = true
            ctx = flag == :rm ? 'image rm' : "buildx bake -f #{shell_quote(dockerfile)}"
          when :save
            values = [['Output', true], 'Platform']
            multiple = true
          when :service
            values = []
            multiple = true
            ctx = 'compose'
          when :connect, :disconnect
            values = ['Options', ['Container', true]]
            ctx = "network #{flag}"
          end
          out, opts, args = choice_index(msg, lines, multiple: multiple, values: values)
          cmd = docker_output(ctx, *action)
          case flag
          when :tag
            args = tagjoin @registry, tag
          when :save
            opts = "#{opts}.tar" unless opts.end_with?('.tar')
            cmd << quote_option('output', File.expand_path(opts))
            if args
              cmd << basic_option('platform', args)
              args = nil
            end
          else
            cmd << opts << '--'
          end
          cmd.merge(Array(out).map! { |val| val.split(/\s+/, 2).first })
          cmd << args
          success?(run(cmd), ctx.start_with?(/network|tag|save/))
        end

        def filetype(val = dockerfile)
          case File.extname(val)
          when '.hcl', '.json'
            bake?(val) ? 1 : 2
          when '.yml', '.yaml'
            if compose?(val)
              @only&.include?('compose') || path.children.none? { |file| bake?(file) } ? 3 : 1
            else
              4
            end
          else
            0
          end
        end

        def contextdir(val = nil)
          val && projectpath?(val) ? shell_quote(basepath(val)) : '.'
        end

        def tagjoin(*args, char: '/')
          args.join(char) unless (args = args.compact).empty?
        end

        def tagname(val)
          val = val.split(':').map! { |s| charname(s.sub(/^\W+/, '')) }
          ret = val.join(':')
          ret = val.first if val.size > 1 && ret.size > 128
          ret[0..127]
        end

        def dnsname(val)
          charname(val[/^[^a-z\d]*(.*?)[^a-z\d]*$/i, 1].gsub(/-{2,}/, '-'))[0..62].downcase
        end

        def charname(val)
          val.gsub(/[^\w.-]+/, '_')
        end

        def tagmain
          tag.is_a?(Array) ? tag.first : tag
        end
      end

      Application.implement Docker
    end
  end

  module Config
    class Viewer
      include Common::Format
      include Utils
      include Rake::DSL

      class << self
        def parse(gem, namespace, ext = [gem])
          require gem
          [eval(namespace), Array(ext)].tap do |data|
            data.last.each { |key| @@mime_obj[key] = data }
          end
        rescue LoadError, NameError => e
          warn e
          nil
        end

        def link(project, main = project.dependfile&.basename, name = nil, **kwargs, &blk)
          return unless project.enabled? && main

          ret = new(main, name, project: project, **kwargs)
          ret.instance_eval(&blk) if block_given?
          ret
        end

        def to_s
          super[/[^:]+\z/, 0]
        end
      end

      @@mime_obj = {}

      attr_reader :main, :name, :project, :theme
      attr_accessor :pipe

      def initialize(main, name = nil, project: nil, command: nil, opts: {}, auto: true,
                     common: ARG[:COMMON], pipe: ARG[:PIPE], **kwargs)
        if project && (project.respond_to?(:workspace) || (project = __get__(:project)[project.to_s]))
          main = project.basepath(main).to_s
          @project = project
          @envname = project.instance_variable_get(:@envname)
          @required = true
        end
        @name = name || @project&.name
        @prefix = kwargs[:prefix] unless @project
        @ext = File.extname(main)
        @dump = kwargs[:dump]
        @mime = {}
        @theme = common ? __get__(:theme)[:viewer] : {}
        @pipe = env_pipe(pipe, @project ? @project.pipe : 1)
        if target?
          @main = main.chomp(@ext)
          @name = @main unless @name || @required
          if auto
            unless command
              command = File.basename(@main)
              command = ARG[:VIEW] if command == @name
            end
            ext = @ext[1..-1].downcase
            if (data = @@mime_obj[ext])
              add(data[1].first, command: command, opts: opts, ext: data[1])
            else
              case ext
              when 'json', 'js'
                add('json', command: command, opts: opts)
              when 'yaml', 'yml'
                add('yaml', command: command, opts: opts)
              end
            end
          end
        else
          @main = main
          @command = command || ARG[:VIEW]
        end
        return unless warning? && ((missing = target? && !File.exist?(main)) || !@name)

        msg, hint = if missing
                      ['path not found', realpath]
                    else
                      @required = true
                      project ? [project, 'missing'] : %w[name missing]
                    end
        warn log_warn(msg, subject: self.class, hint: hint)
      end

      def build
        return unless enabled?

        namespace task_name(name) do |ns|
          @mime.each do |type, items|
            items.each do |command, file, opts|
              next if Rake::Task.task_defined?("#{ns.scope.path}:#{command}:#{type}")

              namespace command do
                unless (data = @@mime_obj[type])
                  ext = [type]
                  ext << 'yml' if type == 'yaml'
                  next unless (data = Viewer.parse(type, type.upcase, ext))
                end
                obj, ext = data
                target = file || (realpath if target?)

                task_desc(command, *ext, target: target)
                task type, [:keys] do |_, args|
                  params = target ? [target, args.to_a] : [args.keys, args.extras]
                  read_keys(obj, ext.first, *params, ext: ext, opts: opts)
                end
              end
            end
          end
        end
        yield self if block_given?
      end

      def add(type, ext: nil, opts: {}, command: ARG[:VIEW], gem: nil, namespace: nil, file: nil)
        return self if @mime.frozen?

        if enabled?
          if namespace
            require(gem || type)
            obj = eval namespace
          else
            Array(ext).each do |val|
              next unless (data = @@mime_obj[val])

              obj = data.first
              break
            end
          end
          if obj
            ext << type if (ext = Array(ext)).empty?
            if !file && target?
              ext.each do |val|
                next unless (out = basepath("#{main}.#{val}")).exist?

                file = out
                break
              end
            end
          end
        end
      rescue LoadError, NameError => e
        log&.warn e
        self
      else
        (@mime[type] ||= []) << [command || @command, file, opts]
        if target?
          @mime[type].freeze
          @mime.freeze
        end
        self
      end

      def also(path, type = nil, name: nil, **kwargs)
        return self unless (file = basepath(path)).exist? && !@mime.frozen?

        ext = mimetype file
        type ||= ext
        name ||= file.basename.to_s.chomp(File.extname(file))
        add(type, ext: ext, command: name, file: file, **kwargs)
      end

      def style(name, *args)
        apply_style theme, name, args
        self
      end

      def enabled?
        return File.exist?(realpath) if target?

        !@required || !!project&.enabled?
      end

      def extensions
        target? ? [@ext.sub('.', '').downcase] : @mime.keys
      end

      def to_s
        realpath if target?

        @mime.keys.map! { |ext| "#{main}.#{ext}" }.join(',')
      end

      def inspect
        "#<#{self.class}: #{name} => #{target? ? realpath : "#{main} {#{extensions.join(', ')}}"}>"
      end

      private

      def puts(*args)
        log_console(*args, pipe: pipe)
      end

      def log
        project&.log
      end

      def read_keys(reader, type, file, keys, ext: [type], opts: {})
        if file && (mime = mimetype(file)) && basepath(file).exist?
          raise_error file, mime, hint: 'invalid' unless ext.include?(mime)
        else
          if ext.include?(mime)
            alt = file
            file = nil
            ext[0] = mime
          elsif file
            keys.unshift(file)
            alt = basepath "#{main}.{#{ext.join(',')}}"
            file = Dir[alt].first
          else
            alt = main
            args = { hint: 'no keys' }
          end
          unless file
            args ||= { kind: Errno::ENOENT }
            raise_error(reader.name, "#{File.basename(alt, '.*')}.#{ext.first}", **args)
          end
        end
        log&.info "#{Viewer}(#{type}) => #{file} {#{keys.join(', ')}}"
        return puts File.read(file) if keys.last == '*'

        doc = if reader.respond_to?(:load_file)
                reader.load_file(file, **opts)
              else
                reader.parse(File.read(file), **opts)
              end
        return unless (lines = print_keys(type, doc, keys, file: file, opts: opts))

        title = File.realpath(file)
                    .sub(/^#{Regexp.escape(File.join(Dir.pwd, ''))}/, '')
        emphasize(lines, title: title, sub: unless stdin?
                                              [
                                                opt_style(theme[:banner], /\A((?:[^:]|(?<! ):(?! ))+)\z/),
                                                opt_style(theme[:undefined], /\A(.*?)(<[^>]+>)(.+)\z/m, 2),
                                                opt_style(theme[:key], /\A((?~ : ))( : (?!undefined).+)\z/m),
                                                opt_style(theme[:number], /\A((?~: ): )(-?[\d.]+)(\s*)\z/m, 2),
                                                opt_style(theme[:string], /\A((?~: ): ")(.+)("\s*)\z/m, 2),
                                                opt_style(theme[:hash], /\A((?~: ): \{)(.+)(\}\s*)\z/m, 2),
                                                opt_style(theme[:array], /\A((?~: ): \[)(.+)(\]\s*)\z/m, 2),
                                                opt_style(theme[:boolean], /\A((?~: ): )(true|false)(\s*)\z/m, 2),
                                                opt_style(theme[:value], /\A((?~: ): (?!undefined))([^"\[{].*)\z/m, 2)
                                              ]
                                            end, border: theme[:border])
      end

      def print_keys(type, data, keys, file: nil, opts: {})
        out = []
        pad = 0
        symbolize = opts[:symbolize_names]
        keys.each do |key|
          begin
            items = key.split('.').flat_map { |name| name =~ /^(.+)\[(\d+)\]$/ ? [$1, $2.to_i] : name }
            items = items.map(&:to_sym) if symbolize
            val = data.dig(*items)
            if val.nil?
              val = data
              items.each do |name|
                raise name unless val.is_a?(Hash) && val.key?(name)

                val = val[name]
              end
            end
          rescue StandardError
            log&.warn "#{Viewer}(#{type}) => #{"#{file} " if file}{#{key}: undefined}"
            val = Regexp.escape($!.message)
            key = key.sub(/(#{val})\.|\.(#{val})|(#{val})/) do
              s = "<#{$3 || $2 || $1}>"
              if $3
                s
              else
                $2 ? ".#{s}" : "#{s}."
              end
            end
            out << [key, stdin? ? JSON.dump(nil) : 'undefined']
          else
            out << [key, @dump == 'json' || stdin? ? JSON.dump(val) : val.inspect]
          end
          pad = [pad, key.size].max
        end
        if stdin?
          puts out.map!(&:last).join("\n")
        else
          out.map! { |a, b| '%-*s : %s' % [pad, a, b] }
        end
      end

      def task_name(val)
        if project
          project.workspace.task_name(val)
        else
          @prefix ? "#{@prefix}:#{val}" : val.to_s
        end
      end

      def task_desc(command, *ext, target: nil)
        return unless Rake::TaskManager.record_task_metadata

        val = "#{ext.first}[#{"file?=#{File.basename(main)}.#{ext.last}," if target}keys+]"
        args = *name.split(':').push(command, val)
        if project
          project.workspace.task_desc(*args)
        else
          desc message(@prefix, *args, empty: true)
        end
      end

      def target?
        !@ext.empty? && (!@required || !project.nil?)
      end

      def warning?
        return true unless project

        project.workspace.warning
      end

      def stdin?
        pipe == 0
      end

      def mimetype(file)
        case (ret = File.extname(file).sub('.', '').downcase)
        when 'yml'
          'yaml'
        when 'js'
          'json'
        when ''
          nil
        else
          ret
        end
      end

      def realpath
        basepath(file = main + @ext).to_s rescue file
      end

      def basepath(*args)
        return Pathname.pwd.join(*args) unless project

        project.basepath(*args)
      end
    end
  end
end

unless defined?(Readline)
  if RUBY_ENGINE == 'ruby' && RUBY_VERSION < '2.7'
    require 'readline'
  else
    begin
      require 'reline'
      Object.send(:remove_const, :Readline) if Object.const_defined?(:Readline)
      Readline = Reline
    rescue LoadError
      require 'readline'
    end
  end
end

Workspace = Squared::Workspace
Project = Squared::Workspace::Project

Viewer = Squared::Config::Viewer

Common = Squared::Common

Common::ARG.update({ PIPE: 'PIPE_STD', OUT: 'PIPE_OUT', FAIL: 'PIPE_FAIL', HOME: 'SQUARED_APP' })

Workspace::Application
  .new(main: 'squared')
  .repo(
    'https://github.com/anpham6/squared-repo', Project::Node.prod? ? 'prod' : 'nightly',
    script: %w[build:dev prod], dev: /^(build:)?dev(:|$)/, ref: %i[base node]
  )
  .with(:node, :python) { clean ['build/'] }
  .with(:node, lint: [nil, Project::Node.prod? ? 'lint' : 'lint:fix'], pass: 'bump') do
    add('e-mc', 'emc', copy: { from: 'publish', scope: '@e-mc', also: %i[pir pir2 express] }) do
      add('publish/*', group: 'emc', pass: %w[install update package bump], exclude: :base)
      revbuild(include: 'src/')
      inject(Viewer, dump: 'json')

      chain('all', :refresh, step: 1)
    end
    add('pi-r', 'pir', graph: 'emc', copy: { from: 'publish', scope: '@pi-r', also: :pir2 }) do
      add('publish/*', group: 'pir', pass: %w[install update package bump], exclude: :base)
      revbuild(include: 'src/')
      inject(Viewer, dump: 'json')

      chain('all', :refresh, after: 'emc')
    end
    add('pi-r2', 'pir2', graph: %w[pir emc], copy: { from: 'publish', workspace: true }) do
      add('publish/*', group: 'pir2', pass: %w[install update package bump], exclude: :base)
      revbuild(include: 'src/')
      inject(Viewer, dump: 'json')

      chain('all', :refresh, with: 'squared')
    end
    add('squared-express', 'express', graph: 'emc', copy: false) do
      add('publish', 'express-prod', group: 'express', only: %w[bump publish pack], exclude: :base)
      revbuild(include: 'src/')
      inject(Viewer, dump: 'json')

      chain('all', :refresh, with: 'pir')
    end
    add('squared', graph: %w[pir express], pass: %w[bump publish pack]) do
      add('ruby', 'squared-rb', group: 'ruby', ref: :ruby)
      add('publish/squared', 'squared-prod', group: 'squared', only: %w[bump publish pack], exclude: :base)
      add('publish/squared-types', 'squared-types', group: 'squared', only: %w[bump publish], exclude: %i[base node])
      add(%w[
        publish/sqd-admin
        publish/sqd-build
        publish/sqd-cli
        publish/sqd-eslint
        publish/sqd-serve
      ], group: 'sqd', pass: %w[install update], exclude: :base)
      revbuild(include: %w[src/ framework/ types/])
      inject(Viewer, 'squared', dump: 'json') do
        add('json')
        add('yaml')
        also('package.json')
        also('tsconfig.json')
      end

      chain('all', :build, after: 'express')
    end
    pass('publish') { parent.nil? }
    banner(:path, styles: %i[yellow bold], border: 'blue')
    banner(:path, styles: %i[magenta bold], border: 'blue', group: 'ruby')
    banner([:name, ': ', :version], styles: %i[magenta bold], border: 'blue', group: %w[emc pir pir2 express squared sqd])
  end
  .with(:python, venv: '.venv', script: false, editable: false) do
    doc(windows? ? '.\make.bat html' : 'make html')
    add 'android-docs'
    add 'chrome-docs'

    chain('all', :doc, with: 'emc')
    banner(:path, styles: %i[blue bold], border: 'blue')
  end
  .with(:docker, run: (false unless ENV['SSH_AUTH_SOCK'] && ENV['GITHUB_TOKEN']), hide: %i[windows? docker?]) do
    add('squared', 'docker', file: ENV['DOCKER_FILE'] ? "#{ENV['DOCKER_FILE']}.Dockerfile" : 'Dockerfile', args: '--ssh=default', secrets: 'id=github,env=GITHUB_TOKEN', pass: 'unpack')
    add('squared', 'docker-test', file: 'docker-bake.hcl', args: '--allow=ssh --allow=fs.read=/tmp --allow=fs.read=/run/user', clean: false, only: %w[build bake])
    add('squared', 'docker-run', file: 'compose.yaml', clean: false, only: %w[build compose])
    banner(:path, styles: %i[cyan bold], border: 'blue')
  end
  .compose('express') do |ns|
    scope = ns.scope.path
    status = lambda do |val|
      ws = Workspace.expect(scope).workspace
      ws.task_desc(*scope.split(':'), val)
    rescue StandardError
      desc 'inactive'
    end

    status.call('copy[ext?=cjs,config?=json,build?]')
    task 'copy' do |_, args|
      exp = Workspace.expect(scope)
      next unless (ws = exp.workspace).home?

      args = args.to_a
      find = ->(*ext, first) { args.find { |val| ext.include?(val.downcase) } || (first && ext.first) }
      dist = exp.path + 'dist'
      src = dist + 'serve.js'
      dest = ws.homepath("serve.#{find.call('cjs', 'js', true)}")
      exp.build(sync: true) if find.call('build', false) || !src.exist?
      cp(src, dest, verbose: ws.verbose)
      if Dir[ws.homepath("#{ws.main}.{yml,yaml,json,js,json5,cjs}")].empty?
        ext = find.call('json', 'yaml', 'yml', true)
        src = Dir[dist + "#{ws.main}*.#{ext == 'yaml' ? 'yml' : ext}"]
        Common::System.copy_guard(src, ws.home, verbose: ws.verbose)
      end
    end

    status.call('serve[opts*=help]')
    task 'serve' do |_, args|
      exp = Workspace.expect(scope)
      ws = exp.workspace
      unless (serve = Dir[ws.homepath('serve.{mjs,cjs,js}')].first)
        exp.build(sync: true)
        Common::Utils.task_invoke('express:copy', **ws.invokeargs)
        serve = 'serve.cjs'
      end
      cmd = ["node #{serve}"] + args.to_a.map! { |val| Common::Shell.fill_option(val) }
      Common::System.shell(cmd.join(' '), chdir: ws.home)
    end
  end
  .chain('all', 'autostash', before: 'emc')
  .style({
    header: %i[yellow bold],
    border: %i[blue],
    inline: %i[cyan bold],
    major: %i[magenta bold],
    latest: %i[blue]
  })
  .git('https://github.com/anpham6', cache: true)
  .build(parallel: ['pull', 'rebase', 'autostash', 'fetch', 'stash', 'clone', 'copy', 'clean', /^outdated:/]) do |ws|
    if !ws.docker? && ws.verbose
      ws.enable_drawing unless ENV['NO_COLOR']
      ws.enable_aixterm if ws.windows? || ENV['TERM']&.end_with?('256color')
    end
    next unless ws.series.some?(:build)

    build = ws.dev?(global: true) && !ws.series.exclude?(:refresh, true) ? 'refresh' : 'build'

    task 'default' => build
    next unless ws.series.some?(:depend)

    ws.task_desc('init')
    task 'init' => ['depend', build]
  end
