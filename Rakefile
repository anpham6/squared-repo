# frozen_string_literal: true

require 'set'
require 'pathname'
require 'logger'
require 'rake'
require 'shellwords'
require 'time'
require 'forwardable'
require 'json'
require 'date'

module Squared
  VERSION = '0.5.21'

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
      GRAPH: ['|', '-', '|', '\\', '-'].freeze,
      BORDER: ['|', '-', '-', '-', '-', '-', '|', '|', '-', '-'].freeze,
      VIEW: 'view',
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
      BOX_GRAPH = ['│', '─', '├', '└', '┬'].freeze
      BOX_BORDER = ['│', '─', '┌', '┐', '┘', '└', '├', '┤', '┬', '┴'].tap do |val|
        if ENV['TERM']&.end_with?('256color')
          val.slice!(2, 4)
          val.insert(2, '╭', '╮', '╯', '╰')
        end
        val.freeze
      end
      TEXT_STYLE = [:bold, :dim, :italic, :underline, :blinking, nil, :inverse, :hidden, :strikethrough].freeze
      private_constant :AIX_TERM, :BOX_GRAPH, :BOX_BORDER, :TEXT_STYLE

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

        if pat && index != 0
          return val unless (data = pat.match(val))

          ret = index == -1 ? data.to_a.drop(1) : data[index]
        else
          ret = val
          index = 0
        end
        wrap = ->(s, n) { "\x1B[#{n.join(';')}m#{s}\x1B[0m" }
        code = []
        args.clear if args.size == 1 && args.first.nil?
        args.concat(Array(styles)).flatten.each_with_index do |type, i|
          next unless type

          if index == -1
            s = ret[i]
            next ret[i] = '' if s.nil?
          else
            s = ret
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
            if (c = __get__(:colors)[t])
              if index == -1
                s = wrap.call(s, [c])
              else
                code << c
              end
            else
              next unless (n = TEXT_STYLE.index(t))

              s = "\x1B[#{n + 1}m#{s}\x1B[#{n == 0 ? 22 : n + 21}m"
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

      def check_style(args, empty: true)
        ret = []
        colors = __get__(:colors)
        Array(args).flatten.compact.each do |val|
          if !val.is_a?(::Numeric)
            val = val.to_sym
            ret << val if colors.key?(val) || TEXT_STYLE.include?(val)
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
          sub = [pat: /\A(#{Regexp.escape(title)})(.*)\z/m, styles: __get__(:theme)[:logger][log_sym(level)]] if color
          emphasize(args, title: title + (subject ? " #{subject}" : ''), sub: sub, pipe: -1)
        else
          msg = [log_title(level, color: color)]
          if subject
            msg << (color ? sub_style(subject.to_s, (@theme.is_a?(::Hash) && @theme[:subject]) || :bold) : subject)
          end
          msg << args.shift if msg.size == 1
          message(msg.join(' '), *args, hint: hint)
        end
      end

      def log_console(*args, pipe: 1)
        return if args.first == false && args.size == 1

        if pipe.is_a?(::Pathname)
          begin
            File.open(pipe, 'a') do |f|
              br = File::SEPARATOR == '\\' ? "\r\n" : "\n"
              args.flatten.each { |val| f.write(strip_style(val.chomp) + br) }
            end
            return
          rescue StandardError
            pipe = 2
          end
        end
        (pipe == 2 ? $stderr : $stdout).puts(*args)
      end

      alias puts_oe log_console

      module_function

      def message(*args, hint: nil, empty: false, space: ARG[:SPACE])
        (empty ? args.reject { |val| val.nil? || (val.respond_to?(:empty?) && val.empty?) } : args)
          .join(space) + (hint ? " (#{hint})" : '')
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
        n = cols || max.call(lines)
        if $stdout.tty?
          require 'io/console'
          (n = [n, $stdout.winsize[1] - 4].min) rescue nil
        end
        b0, b1, b2, b3, b4, b5, b6, b7 = ARG[:BORDER]
        out = []
        draw = lambda do |a, b|
          ret = a + (b1 * (n + 2)) + b
          ret = sub_style(ret, styles: border) if border
          ret
        end
        sub = sub.is_a?(::Hash) ? [sub] : Array(sub)
        pr = lambda do |line|
          s = line.ljust(n)
          sub.each { |h| s = sub_style(s, **h) }
          s = "#{b0} #{s} #{b0}"
          if border
            s = sub_style(s, pat: /\A(#{Regexp.escape(b0)})(.+)\z/om, styles: border)
            s = sub_style(s, pat: /\A(.+)(#{Regexp.escape(b0)})\z/om, styles: border, index: 2)
          end
          s
        end
        out << draw.call(b2, b3)
        if title
          out.concat(title.map { |t| pr.call(t) })
          out << draw.call(b6, b7)
        end
        lines.each { |line| out << pr.call(line) }
        out << draw.call(b5, b4)
        if footer
          unless sub.empty? && !right
            footer.map! do |s|
              s = s.rjust(n + 4) if right
              sub.each { |h| s = sub_style(s, **h) }
              s
            end
          end
          out.concat(footer)
        end
        if block_given?
          yield out
        elsif pipe
          case pipe
          when -1
            return out
          when 0
            pipe = $stdin
          when 2
            pipe = $stderr
          else
            pipe = $stdout unless pipe.respond_to?(:puts)
          end
          pipe.puts(out)
        else
          err ? warn(out) : puts(out)
        end
      end

      def strip_style(val)
        val.gsub(/\x1B\[(\d+;?)+m/, '')
      end

      def stripext(val)
        File.basename(val, '.*')
      end

      def raise_error(*args, hint: nil, kind: ArgumentError)
        raise kind, message(*args, hint: hint, empty: true), caller_locations(1).map(&:to_s)
      end
    end

    module Prompt
      module_function

      def confirm(msg, default = nil, agree: 'Y', cancel: 'N', attempts: 5, timeout: 30)
        require 'readline'
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
              return false
            end
            attempts -= 1
            exit 1 unless attempts > 0
          end
        rescue Interrupt
          puts
          exit 0
        else
          false
        end
      end

      def choice(msg, list = nil, min: 1, max: 1, multiple: false, force: true, grep: nil, auto: true,
                 attempts: 5, timeout: 0)
        require 'readline'
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
          raise_error 'empty selection list' if max == 0
          min = grep ? 1 : [min, max].min
          if auto
            msg = "#{msg}: [#{min}-#{max}#{if multiple
                                             "|,#{multiple.is_a?(::Numeric) ? "{#{multiple}}" : ''}"
                                           end}] "
          end
        end
        valid = ->(s) { s.match?(/^\d+$/) && s.to_i.between?(min, max) }
        Timeout.timeout(timeout) do
          while (ch = Readline.readline(msg))
            unless (ch = ch.strip).empty?
              if multiple
                a = ch.split(',')
                b = a.map do |s|
                  s.strip!
                  if s =~ /^(\d+)-(\d+)$/
                    next unless valid.call($1) && valid.call($2)

                    c = $1.to_i
                    d = $2.to_i
                    next (c..d).to_a if c < d
                  elsif valid.call(s)
                    s.to_i
                  end
                end
                unless b.include?(nil)
                  b.flatten!
                  b.uniq!
                  b.sort!
                  return items ? b.map! { |i| items[i - 1] } : b unless multiple.is_a?(::Numeric) && multiple != b.size
                end
              elsif valid.call(ch)
                return items ? items[ch.to_i - 1] : ch.to_i
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
          multiple ? [] : nil
        end
      end

      def readline(msg, history = false, force: nil, multiline: nil, &blk)
        require 'readline'
        multiline = if multiline && Readline.respond_to?(:readmultiline)
                      multiline.is_a?(::Enumerable) || block_given? ? multiline : [multiline.to_s]
                    end
        prompt = lambda do
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
          msg = "#{msg} %s " % if multiline
                                 multiline.is_a?(::Enumerable) ? "{#{multiline.to_a.join('|')}}" : multiline
                               else
                                 "(#{force ? 'required' : 'optional'}):"
                               end
          ret = (prompt.call || '').strip
          multiline.each { |val| break if ret.delete_suffix!(val.to_s) } if multiline.is_a?(::Enumerable)
          exit 1 if force && ret.empty?
          ret
        else
          prompt.call
        end
      end
    end

    module Shell
      QUOTE_VALUE = /\A(["'])(.*)\1\z/m.freeze
      private_constant :QUOTE_VALUE

      module_function

      def shell_escape(val, quote: false, force: false, double: false, option: false, override: false)
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

      def shell_option(flag, val = nil, escape: true, quote: true, option: true, force: true, double: false,
                       merge: false, override: false)
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
                    '='
                  else
                    merge ? '' : ' '
                  end
                elsif flag.size == 1
                  pre = '-'
                  merge ? '' : ' '
                else
                  pre = '--'
                  '='
                end
              end
        "#{pre}#{flag}#{unless val.nil?
                          "#{sep}#{if escape
                                     shell_escape(val, quote: quote, double: double, override: override)
                                   elsif quote
                                     shell_quote(val, option: option, force: force, double: double, override: override)
                                   else
                                     val
                                   end}"
                        end}"
      end

      def shell_split(val, join: nil, **kwargs)
        ret = val.shellsplit.map! { |opt| shell_escape(opt, double: true, option: true, **kwargs) }
        return ret unless join

        ret.join(join.is_a?(::String) ? join : ' ')
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

      def copy_dir(src, dest, glob = ['**/*'], create: false, link: nil, force: false, pass: nil, hidden: false,
                   verbose: true)
        base = Pathname.new(src)
        target = Pathname.new(dest)
        raise "#{target.cleanpath} (not found)" if !create && !target.parent.exist?

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
        subdir.each do |dir, files|
          if link
            files.dup.tap do |items|
              files.clear
              items.each do |file|
                if file.exist?
                  if !file.symlink?
                    files << file
                  elsif !force
                    next
                  end
                end
                if link == 'hard'
                  FileUtils.ln(file, dir, force: force, verbose: false)
                else
                  FileUtils.ln_s(file, dir, force: force, verbose: false)
                end
                soft += 1
              end
            end
          end
          next if files.empty?

          out = FileUtils.cp(files, dir, verbose: false)
          count += out.size
        end
        puts [target.realpath, subdir.size, soft > 0 ? "#{count}+#{soft}" : count].join(' => ') if verbose
      end

      def copy_guard(src, dest, link: nil, force: false, verbose: true)
        unless force
          target = Pathname.new(dest)
          if target.directory?
            src = Array(src).reject { |val| target.join(File.basename(val)).exist? }
            return if src.empty?
          elsif target.exist?
            return
          end
        end
        case link
        when 'hard', 1
          FileUtils.ln(src, dest, force: force, verbose: verbose)
        when ::TrueClass, 'soft', 0
          FileUtils.ln_s(src, dest, force: force, verbose: verbose)
        else
          FileUtils.cp(src, dest, verbose: verbose)
        end
      end
    end

    module Utils
      module_function

      def split_escape(val, char: ',')
        val.split(/\s*(?<!\\)#{char}\s*/)
      end

      def split_option(val)
        val = val.strip
        return [val, '', ''] unless (i = val.index('='))

        last = val[(i + 1)..-1].strip
        quote = ''
        if last =~ /\A(["'])(.+)\1\z/
          last = $2
          quote = $1
        end
        [val[0..(i - 1)], last, quote]
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

      def env(key, default = nil, suffix: nil, strict: false, equals: nil, ignore: nil)
        ret = env_value(key, suffix: suffix, strict: strict)
        return ret == equals.to_s unless equals.nil?

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
      class << self
        def hashobj
          Hash.new { |data, key| data[key] = {} }
        end

        def hashlist
          Hash.new { |data, key| data[key] = [] }
        end

        def hashdup(data, pass: {})
          ret = {}
          data.each do |key, val|
            ret[key] = case val
                       when Hash
                         pass[val] ||= hashdup(val, pass: pass)
                       when Proc, Method
                         val
                       else
                         val.dup
                       end
          end
          ret
        end
      end

      RunData = Struct.new('RunData', :run, :block)
      ChainData = Struct.new('ChainData', :action, :step, :with, :before, :after, :sync)
      BannerData = Struct.new('BannerData', :command, :order, :styles, :border)
    end

    class Application
      include Common::Format
      include Utils
      include Rake::DSL

      SCRIPT_OBJ = {
        run: nil,
        script: nil,
        dev: nil,
        prod: nil,
        global: false,
        env: false
      }.freeze
      TASK_METADATA = Rake::TaskManager.record_task_metadata
      private_constant :SCRIPT_OBJ, :TASK_METADATA

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
              obj.tasks&.each { |task| impl_series.add(task, obj) }
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

        def series_wrap(app)
          impl_series.new(app, exclude: @task_exclude.to_a)
        end

        def baseref
          impl_project.ref
        end

        def to_s
          super[/[^:]+\z/, 0]
        end

        attr_reader :kind_project
        attr_accessor :impl_series, :impl_project, :attr_banner
      end

      @kind_project = []
      @task_exclude = Set.new

      attr_reader :root, :home, :main, :prefix, :exception, :warning, :pipe, :verbose, :theme, :series, :closed

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
        @series = Application.series_wrap(self)
        @project = {}
        @kind = Support.hashlist
        @extensions = []
        @envname = env_key(@main).freeze
        @pipe = $DEBUG ? 2 : env_pipe(pipe, (ARG[:OUT] && env(ARG[:OUT])) || 1, root: @home)
        @exception = env_bool exception
        @verbose = if $VERBOSE.nil?
                     false
                   elsif verbose.nil?
                     @pipe != 0
                   else
                     env_bool(verbose, verbose.is_a?(String) ? @pipe != 0 : verbose, index: true)
                   end
        @warning = @verbose != false
        @closed = false
        if common
          @theme = __get__(:theme)[:workspace]
          ARG[:COLOR] = false if @pipe == 0 || @pipe.is_a?(Pathname)
        else
          @theme = {}
        end
        @chain = Support.hashlist
        @script = {
          group: Support.hashobj,
          ref: Support.hashobj,
          group!: {},
          ref!: {}
        }.freeze
        @events = {
          group: Support.hashobj,
          ref: Support.hashobj
        }.freeze
        @pass = {
          group: Support.hashobj,
          ref: Support.hashobj,
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

      def build(parallel: [], pass: nil, **kwargs)
        return self unless enabled? && !@closed

        kwargs[:parallel] = if kwargs[:pattern].is_a?(Array)
                              parallel.map(&:to_s)
                            else
                              kwargs[:pattern] = []
                              parallel.reject { |val| kwargs[:pattern] << val if val.is_a?(Regexp) }.map!(&:to_s)
                            end
        @pass[:pattern].concat(pass.map { |val| val.is_a?(Regexp) ? val : val.to_s }) if pass
        @project.each_value do |proj|
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

      def with(*val, pass: false, group: nil, **kwargs, &blk)
        return self if pass == true || (pass && as_a(pass, :to_s).any? { |s| respond_to?(s) && __send__(s) rescue nil })

        @group = nil
        @ref = nil
        @withargs = kwargs.empty? ? nil : kwargs
        val = as_a(group || kwargs[:ref], flat: true, compact: true) if val.empty?
        kind = val.first
        val = kind if val.size == 1
        case kind
        when String
          @group = val
        when Symbol
          @ref = val
        else
          raise_error 'missing group or ref' if block_given?
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
        keys = if project
                 action.map! { |val| task_join(project.name, val) }
                 nil
               elsif (target = group || ref)
                 action.map! { |val| task_name(task_join(val, target)) }
                 nil
               else
                 action.map! { |val| task_name(val) }
                 prefix ? nil : @project.keys
               end
        ns = lambda do |val|
          next if (ret = as_a(val, :to_s, flat: true)).empty?

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
        if group
          label = :group
          items = Array(group)
        else
          label = :ref
          items = Array(ref || :_)
        end
        items.each { |val| @banner[label][val.to_sym] = data }
        self
      end

      def add(path, project = nil, **kwargs, &blk)
        kwargs = Support.hashdup(@withargs).update(kwargs) if @withargs
        ref = kwargs.key?(:ref) ? kwargs.delete(:ref) : @ref
        kwargs[:group] = @group if @group && !kwargs.key?(:group)
        path = root + path
        project = (project || path.basename).to_s
        name = task_name project
        index = 0
        while @project[name]
          index += 1
          name = task_name "#{project}-#{index}"
        end
        proj = ((if !ref.is_a?(Class)
                   Application.find(ref, path: path)
                 elsif ref < Application.impl_project
                   ref
                 end) || @kind[name]&.last || Application.impl_project).new(self, path, name, **kwargs)
        proj.__send__(:index_set, @project.size)
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
          begin
            obj = JSON.parse(homepath(obj).read, { symbolize_names: true })
          rescue StandardError => e
            warn log_message(Logger::ERROR, e)
            obj = nil
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
            @describe[val.is_a?(Regexp) ? :pattern : :alias][key.to_s] = val
          end
        end
        self
      end

      def find(path = nil, name: nil, group: nil, ref: nil, &blk)
        ret = group ? @project.select { |_, item| item.group == group }.map(&:last) : []
        if path.is_a?(Symbol)
          ref ||= path
          path = nil
        end
        if ret.empty?
          ret = @project.select { |_, item| item.ref?(ref) }.map(&:last) if ref
          if ret.empty? && (path || name)
            path &&= rootpath path
            name &&= name.to_s
            proj = @project.find { |_, item| (path && item.path == path) || (name && item.name == name) }&.last
            ret << proj if proj
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
        prefix && val.is_a?(String) ? val.sub(/\A#{Regexp.escape(prefix)}:/, '') : val.to_s
      end

      def task_desc(*args, **kwargs)
        return unless TASK_METADATA

        name = kwargs.delete(:name)
        if @describe
          val = name || task_join(*args)
          found = false
          replace = lambda do |data, out|
            index = data.size
            data.to_a.reverse_each { |group| out.sub!("%#{index -= 1}", group) }
            out
          end
          @describe[:replace].each do |pat, tmpl|
            next unless val =~ pat

            val = replace.call($~, tmpl.dup)
            found = true
          end
          if (out = @describe[:alias][val])
            val = out
            found = true
          else
            @describe[:pattern].each do |key, pat|
              next unless val =~ pat

              val = replace.call($~, key.dup)
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
          next unless val && (ret = val.is_a?(Symbol) ? @script[:ref!][val] : @script[:group!][val.to_sym])

          return ret
        end
        @script[:ref!][:''] || SCRIPT_OBJ
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
        !@extensions.empty? || @project.values.any? { |proj| proj.enabled?(base: false) }
      end

      def task_base?(key)
        series.base?(key)
      end

      def task_extend?(obj, key)
        series.extend?(obj, key)
      end

      def task_include?(obj, key, ref = nil)
        return false if @series.exclude?(key)

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
                    with = lambda do |n|
                      tasks.insert(n, *data.action)
                      sync << tasks
                      data.action.clear
                    end
                    tasks&.each_with_index do |v1, l|
                      index = k if w && has.call(w, v1)
                      if a && has.call(a, v1)
                        if index
                          with.call(l + 1)
                          throw :found
                        else
                          index = k + 1
                        end
                      elsif b && has.call(b, v1)
                        if index
                          with.call(l)
                          throw :found
                        else
                          index = k - 1
                        end
                      elsif index
                        if a || b
                          tasks.each_with_index do |v2, m|
                            if a && has.call(a, v2)
                              with.call(m + 1)
                              throw :found
                            elsif b && has.call(b, v2)
                              with.call(m)
                              throw :found
                            end
                          end
                          if !pass
                            pass = [i, data]
                          elsif pass.include?(data)
                            if i == pass.first + 1
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
                      step = index == -1 ? -1 : index + 1
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
              puts(log_message(Logger::ERROR, *(failed + group.map { |val| val.action }.flatten),
                               subject: 'failed placement', hint: false), pipe: 2)
            end
            cols = level.flatten(1).map(&:size).max
            level.each_with_index do |grp, n|
              title = "Step #{n.succ}#{if !sync.include?(grp) || (grp.size == 1 && series.parallel.include?(grp.first))
                                         ''
                                       else
                                         ' (sync)'
                                       end}"
              emphasize(grp, title: title, cols: [cols, title.size].max, border: theme[:border],
                             sub: [pat: /\A(Step \d+)(.*)\z/, styles: theme[:header]])
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
          items = as_a(group, :to_sym)
        else
          label = :ref
          items = as_a(ref, :to_sym)
        end
        items.each do |name|
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

      def root?(path, pass: [])
        return false unless path.directory?

        path.each_child do |c|
          name = c.basename.to_s
          next if c.to_s == __FILE__ || (@main == name && c.directory? && c.empty?) || pass.any? { |val| val == name }

          return false
        end
        true
      end

      def script?(state, target: nil, pat: nil, group: nil, ref: baseref, global: false)
        data = script_find ref, group
        if global
          target = data[:script] || data[:run] if target.nil?
          pat = data[state] if pat.nil?
        end
        return false if state == :prod && data[:dev] == true && data[:global]

        target && pat.is_a?(Regexp) ? Array(target).any?(pat) : pat == true
      end

      def scriptobj
        SCRIPT_OBJ.dup
      end
    end

    class Series
      include Rake::DSL
      extend Forwardable

      TASK_BASE = []
      TASK_BATCH = {}
      TASK_EXTEND = Support.hashlist
      TASK_KEYS = []
      TASK_ALIAS = Support.hashobj
      TASK_NAME = {}
      private_constant :TASK_BASE, :TASK_BATCH, :TASK_EXTEND, :TASK_KEYS, :TASK_ALIAS, :TASK_NAME

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

        private

        def key_set(val)
          return if TASK_KEYS.include?(val)

          TASK_KEYS << val
          TASK_BASE.delete(val)
        end
      end

      attr_reader :sync, :multiple, :parallel

      def_delegators :@data, :[], :each, :each_key, :keys, :key?, :fetch, :update, :merge!, :to_a, :to_s, :inspect
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
        @data = {}
        (TASK_BASE + TASK_KEYS).each { |key| @data[key] = [] }
      end

      def populate(proj, **)
        group, parent, id = @session.values
        ws = proj.workspace
        @data.each do |key, items|
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
        @data.update(@session[:parent]) if @session[:id].uniq.size > 1
        @data.update(@session[:group])
        @data.each do |key, items|
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
        return @data.key?(key) && !@data[key].empty? unless (batch = batch_get(key))

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

        meth = :"#{key}?"
        ret = false
        TASK_EXTEND[key].each do |kind|
          next unless obj.is_a?(kind)

          if kind.method_defined?(meth)
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
        @exclude.include?(key) || (empty && (!@data.key?(key) || @data[key].empty?))
      end

      private

      def invoked_get(name)
        return unless Rake::Task.task_defined?(name) && (ret = Rake::Task[name]).already_invoked

        ret
      end

      def already_invoked?(list, val)
        return Rake::Task.tasks.any? { |obj| obj.already_invoked && list.include?(obj.name) } unless val

        list.include?(val) && !invoked_get(val).nil?
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
                       **)
              return if (ret = args.flatten).empty?

              target << '--' if delim && !target.include?('--')
              if strip
                pat, s = Array(strip)
                ret.map! { |val| val.is_a?(String) ? val.gsub(pat, s || '') : val }
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
              ret
            end

            def clear(target, opts, pass: true, styles: nil, **kwargs)
              return if opts.empty?

              kwargs[:subject] ||= stripext target.first
              kwargs[:hint] ||= 'unrecognized'
              append(target, opts, delim: true) if kwargs.delete(:append)
              warn log_message(Logger::WARN, opts.join(', '), pass: true, **kwargs)
              exit 1 unless pass || confirm("Run? [#{sub_style(target, styles: styles)}]", 'N')
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
              return [] unless val

              val = shell_split val if val.is_a?(String)
              val.map { |s| s.sub(OPT_SINGLE, '\1=\2').sub(OPT_VALUE, '\1=\2').sub(OPT_NAME, '\2') }.reject(&:empty?)
            end

            def select(list, bare: true, no: true, single: false, double: false)
              ret = bare ? list.grep_v(/=/) : list.grep(/=/).map! { |val| val.split('=', 2).first }
              ret.map! { |val| val.split('|', 2).last }
              ret = ret.grep_v(/\Ano-/) unless no
              return ret if single == double

              ret.select { |val| single ? val.size == 1 : val.size > 1 }
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
              val.match?(/(?:\A\^|\$\z)/) || val.match?(/(?:\.[*+]|\(\?:|\\[dsw]|\[.+\]|\{\d+,?\d*})/)
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

          attr_reader :target, :extras, :found, :errors, :values, :project, :path

          def_delegators :@target, :+, :-, :<<, :any?, :none?, :include?, :add, :add?, :find, :find_all, :find_index,
                         :merge, :compact, :delete, :delete?, :delete_if, :grep, :grep_v, :inspect, :to_a, :to_s
          def_delegators :@extras, :empty?, :member?, :each, :each_with_index, :each_with_object, :partition, :dup,
                         :first, :shift, :unshift, :pop, :push, :concat, :index, :join, :map, :map!, :detect, :select,
                         :select!, :reject, :size

          def_delegator :@extras, :delete, :remove
          def_delegator :@extras, :delete_at, :remove_at
          def_delegator :@extras, :delete_if, :remove_if
          def_delegator :@extras, :find_all, :detect_all
          def_delegator :@extras, :find_index, :detect_index

          def initialize(opts, list, target = Set.new, project: nil, path: nil, **kwargs, &blk)
            @target = target.is_a?(Set) ? target : target.to_set
            @project = project
            @path = path || project&.path
            @errors = []
            @found = []
            parse(list, opts, **kwargs, &blk)
          end

          def parse(list, opts = extras, no: nil, single: nil, args: false, first: nil, underscore: nil, &blk)
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
                case val[n + 1]
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
                else
                  next
                end
                m << flag if flag.size == 1 && val[n + 2] == 'm'
                bare << flag if val.end_with?('?')
              else
                bare << val
              end
            end
            no = (no || []).map { |val| (n = val.index('=')) ? val[0, n] : val }
            bare.concat(no)
            if underscore
              tr = ->(list) { list.map { |val| val.tr('-', '_') } }
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
              no.concat(tr.call(no))
            end
            numtype = [
              [i, /\A\d+\z/],
              [f, /\A\d*(?:\.\d+)?\z/],
              [si, /\A-?\d+\z/]
            ].freeze
            numcheck = ->(k, v) { numtype.any? { |flag, pat| flag.include?(k) && pat.match?(v) } }
            skip = false
            opts.each do |opt|
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
                    add shell_option(key, val, merge: merge)
                  elsif q.include?(key)
                    add quote_option(key, val, double: qq.include?(key), merge: merge)
                  elsif p.include?(key)
                    if val.match?(/\A(["']).+\1\z/)
                      add shell_option(key, val, escape: false, merge: merge)
                    elsif path
                      add quote_option(key, path + val, merge: merge)
                    else
                      push opt
                    end
                  elsif b.include?(key) || (bl.include?(key) && %w[true false].include?(val)) || numcheck.call(key, val)
                    add basic_option(key, val, merge: merge)
                  elsif merge
                    add basic_option(key, val, merge: true)
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
            @values = @values.empty? ? /\A\s+\z/ : /\A(#{@values.join('|')})=(.+)\z/m
            @extras.each_with_index(&blk) if block_given?
            self
          end

          def swap(opts = nil)
            unless opts
              opts = found
              @found = []
            end
            @extras = opts
            self
          end

          def append(*args, **kwargs)
            args = extras if args.empty?
            OptionPartition.append(target, *args, **kwargs)
            self
          end

          def append_any(*args, quote: true, **kwargs)
            (args.empty? ? extras : args.flatten).each do |val|
              if exist?(val)
                add_path(val, **kwargs)
              elsif quote
                add_quote(val, **kwargs)
              else
                add val
              end
              found << val if args.empty?
            end
            self
          end

          def delete_key(*args, **kwargs)
            OptionPartition.delete_key(target, *args, **kwargs)
            self
          end

          def values_of(*args, strict: true, first: false, last: false)
            eq, s = strict ? ['=', '[^ ]+'] : ['(?:=| +)', '[^-][^ ]*']
            g = ["\"((?:[^\"]|(?<=\\\\)\"(?!$#{windows? ? '| ' : ''}))*)\""]
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
              OptionPartition.clear(target, self.errors, styles: styles, **kwargs)
              self.errors.clear
              return self unless opts
            end
            opts ||= extras
            OptionPartition.clear(target, if found.empty?
                                            opts
                                          else
                                            opts.reject { |val| found.include?(val) }
                                          end, styles: styles, **kwargs)
            opts.clear
            self
          end

          def adjoin(*args, with: nil, start: false)
            i = -1
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
              temp.each_with_index do |val, index|
                if val.to_s.match?(pat)
                  i = index + (start.is_a?(Numeric) ? start : 1)
                  break
                end
              end
            else
              temp.each_with_index do |val, index|
                if i == 0
                  next unless val.is_a?(String) && val.start_with?('-')

                  i = index
                  break
                elsif index > 0 && !val.to_s.start_with?('-')
                  if start
                    i = index + (start.is_a?(Numeric) ? start : 1)
                    break
                  end
                  i = 0
                end
              end
            end
            if i > 0
              if args.empty?
                args = dup
                reset
              else
                args.each { |val| remove val }
              end
              args = temp[0...i] + args + temp[i..-1]
              target.clear
            end
            merge args
            self
          end

          def add_path(*args, **kwargs)
            add shell_quote(path ? path.join(*args) : File.join(*args), option: false, **kwargs)
            self
          end

          def add_quote(*args, **kwargs)
            args.compact!
            merge(args.map! { |val| val == '--' || OptionPartition.opt?(val) ? val : shell_quote(val, **kwargs) })
            self
          end

          def delim
            add '--'
            self
          end

          def splice(*exclude, quote: true, delim: true, path: false, pattern: false, &blk)
            temp, other = if block_given?
                            partition(&blk)
                          elsif exclude.first.is_a?(Symbol)
                            partition(&exclude.first)
                          else
                            partition do |val|
                              next false if pattern && OptionPartition.pattern?(val)

                              exclude.none? { |pat| val.match?(Regexp.new(pat)) }
                            end
                          end
            unless temp.empty?
              add '--' if delim
              extras.clear
              concat other
              if path
                temp.each { |val| add_path(val) }
              else
                temp.map! { |val| shell_quote(val) } if quote
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

          def append?(key, val = nil, type: nil, force: false, **kwargs)
            return false unless force || !arg?(key)

            val = yield self if block_given?
            return false unless val

            type ||= :quote if kwargs.empty?
            add case type
                when :quote
                  quote_option(key, val)
                when :basic
                  basic_option(key, val)
                else
                  shell_option(key, val, **kwargs)
                end
            true
          end

          def arg?(*args, **kwargs)
            OptionPartition.arg?(target, *args, **kwargs)
          end

          def exist?(*args, add: false, first: false, last: false)
            return false unless path
            return path.join(*args).exist? unless args.empty?

            if first || last
              return false unless (val = first ? self.first : self.last).is_a?(String)

              path.join(val).exist?.tap do |ret|
                next unless add && ret

                add_path(first ? shift : pop)
              end
            else
              each_with_index do |val, index|
                next unless val.is_a?(String) && path.join(val).exist?

                if add
                  remove_at index
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

          attr_reader :delim, :extras

          def initialize(data = [], delim: ' ', partition: '--', uniq: /\A--?[^=\s-][^=\s]*(?:=|\s+)\S/)
            @delim = delim
            @partition = partition
            @uniq = uniq
            @extras = []
            super(data.compact)
          end

          def compact
            to_ary.map!(&:to_s).reject(&:empty?)
          end

          def last(val, pat)
            (@last ||= []).push([val, pat, $1]) if val =~ pat
            self << val
          end

          def pass(&blk)
            ret = compact
            @last&.each do |val, pat, key|
              i = []
              j = nil
              ret.each_with_index do |opt, index|
                if opt == val
                  j = index
                elsif j && opt[pat, 1] == key
                  i << index
                end
              end
              next unless j && !i.empty?

              val = ret[j]
              cur = j
              i.each do |k|
                ret[cur] = ret[k]
                cur = k
              end
              ret[i.last] = val
            end
            ret.concat(extras.map(&:to_s).reject(&:empty?)) unless extras.empty?
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
              @extras = if n == 0
                          data
                        else
                          super(data[0...n])
                          data[n..-1]
                        end
              self
            else
              super
            end
          end

          def <<(obj)
            extras!(obj) || super
          end

          def size
            super + extras.size
          end

          def include?(obj)
            return true if super
            return extras.include?(obj) unless (n = extras.index(@partition))

            extras[0..n].include?(obj)
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
          alias member? include?

          private

          def extras!(obj)
            return if extras.empty? && !extras?(obj)

            extras << obj unless !extras.include?(@partition) && include?(obj) && @uniq.match?(obj.to_s)
            self
          end

          def extras?(obj)
            obj == @partition || (include?(obj) && !@uniq.match?(obj.to_s))
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
        include Rake::DSL

        VAR_SET = %i[parent global script index envname desc dependfile dependindex theme archive env dev prod graph
                     pass only exclude asdf].freeze
        BLK_SET = %i[run depend doc lint test copy clean].freeze
        SEM_VER = /\b(\d+)(?:(\.)(\d+))?(?:(\.)(\d+))?[-.]?(\S+)?\b/.freeze
        URI_SCHEME = %r{\A([a-z][a-z\d+-.]*)://[^@:\[\]\\^<>|\s]}i.freeze
        TASK_METADATA = Rake::TaskManager.record_task_metadata
        private_constant :VAR_SET, :BLK_SET, :SEM_VER, :URI_SCHEME, :TASK_METADATA

        class << self
          def populate(*); end
          def batchargs(*); end
          def aliasargs(*); end
          def bannerargs(*); end

          def tasks
            (%i[build archive graph prereqs] + BLK_SET).freeze
          end

          def as_path(val)
            case val
            when Pathname
              val
            when String
              Pathname.new(val)
            end
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
        end

        @@tasks = {}
        @@graph = { _: [] }
        @@asdf = Pathname.new("#{Dir.home}/.asdf").yield_self do |path|
          if path.join('asdf.sh').exist?
            [path, 15]
          elsif ENV['ASDF_DATA_DIR']
            [Pathname.new(ENV['ASDF_DATA_DIR']), 16]
          end
        end
        @@print_order = 0

        subtasks({
          'graph' => %i[run print].freeze,
          'unpack' => %i[zip tar gem ext].freeze,
          'asdf' => %i[set exec current]
        })

        attr_reader :name, :workspace, :path, :theme, :group, :parent, :dependfile,
                    :exception, :pipe, :verbose
        attr_accessor :global, :project

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
          @output = []
          @ref = []
          @children = []
          @events = hashobj.update({ first: first, last: last, error: error })
          @as = hashobj
          @desc = (@name.include?(':') ? @name.split(':').join(ARG[:SPACE]) : @name).freeze
          @parent = nil
          @global = false
          @log = nil
          @dev = nil
          @prod = nil
          @withargs = nil
          @session = nil
          @index = -1
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
            if (scr = data[:script])
              unless kwargs[:script] == false
                @global = true
                script_set(scr, args: data.fetch(:args, kwargs[:args]), prod: kwargs[:prod])
              end
            elsif (run = data[:run])
              @global = true
              run_set run
            end
            unless data[:env]
              if (scr = kwargs[:script])
                @global = false
                script_set(scr, args: kwargs[:args])
              elsif @script && !data[:global]
                if (scr = @script[:script])
                  @global = false
                  script_set(scr, args: @script.fetch(:args, kwargs[:args]))
                elsif (run = @script[:run])
                  @global = false
                  run_set run
                end
              end
            end
          elsif data[:env] && data[:run]
            @global = true
            run_set data[:run]
          end
        end

        def initialize_events(ref, **)
          return unless (events = @workspace.events_get(group: @group, ref: ref))

          events.each { |task, data| data.each { |ev, blk| @events[ev][task] ||= [blk] } }
        end

        def initialize_logger(log: nil, **)
          return if @log

          log = log.is_a?(Hash) ? log.dup : { file: log }
          if (file = env('LOG_FILE'))
            file = Time.now.strftime(file)
          elsif (val = env('LOG_AUTO'))
            file = "#{@name}-%s.log" % [case val
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
            file = val.is_a?(String) ? Time.now.strftime(val) : "#{@name}-#{Date.today}.log"
          end
          begin
            file &&= @workspace.home.join(env('LOG_DIR', ''), file).realdirpath
          rescue StandardError => e
            file = nil
            print_error e
          end
          log[:progname] ||= @name
          if (val = env('LOG_LEVEL', ignore: false))
            log[:level] = val.match?(/\d/) ? log_sym(val.to_i) : val
          end
          log.delete(:file)
          @log = [file, log]
        end

        def initialize_env(dev: nil, prod: nil, **)
          @dev = env_match('BUILD', dev, suffix: 'DEV', strict: true)
          @prod = env_match('BUILD', prod, suffix: 'PROD', strict: true)
          if (val = env('BUILD', suffix: 'ENV')) && @output[2] != false
            @output[2] = parse_json(val, hint: "BUILD_#{@envname}_ENV") || @output[2]
          end
          unless @output[0] == false || @output[0].is_a?(Array)
            if (val = env('BUILD', suffix: 'OPTS'))
              n = @output[0] ? 1 : 3
              @output[n] = merge_opts(@output[n], shell_split(val))
            end
            if (val = env(ref.to_s.upcase, suffix: 'OPTS'))
              @output[4] = merge_opts(@output[4], shell_split(val))
            end
          end
          @version = val if (val = env('BUILD', suffix: 'VERSION'))
          return unless (val = env('BUILD', strict: true))

          @global = false
          if val == '0'
            @output = [false]
          elsif script?
            script_set val
          else
            run_set val
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
                        out, done = graph(args, out: [])
                        out.map! do |val|
                          n = done.index { |proj| val.match?(/ #{Regexp.escape(proj.name)}(?:@\d|\z)/) }
                          n ? "#{val} (#{n.succ})" : val
                        end
                        emphasize(out, title: path, right: true, border: borderstyle, sub: [
                          { pat: /\A(#{Regexp.escape(path.to_s)})(.*)\z/, styles: theme[:header] },
                          { pat: /\A(#{Regexp.escape(name)})(.*)\z/, styles: theme[:active] },
                          { pat: /\A((?~ \() \()(\d+)(\).*)\z/, styles: theme[:inline], index: 2 }
                        ])
                      end
                    end
                  when 'unpack'
                    format_desc(action, flag, 'tag/url,dir,digest?,f|force?', before: flag == :ext ? 'ext' : nil)
                    params = %i[tag dir digest force]
                    params.unshift(:ext) if flag == :ext
                    task flag, params do |_, args|
                      ext = flag == :ext ? param_guard(action, flag, args: args, key: :ext) : flag.to_s
                      tag = param_guard(action, flag, args: args, key: :tag)
                      dir = param_guard(action, flag, args: args, key: :dir)
                      unless tag.match?(URI_SCHEME)
                        if flag == :gem
                          tag = "https://rubygems.org/downloads/#{File.basename(tag, '.gem')}.gem"
                        elsif @release
                          tag = "%s.#{ext}" % [@release.include?('??') ? @release.sub('??', tag) : @release + tag]
                        else
                          raise_error("no base uri: #{tag}", hint: ext)
                        end
                      end
                      case (digest = args.digest)
                      when 'f', 'force'
                        digest = nil
                        force = true
                      else
                        force = args.fetch(:force, false)
                      end
                      unpack(basepath(dir), uri: tag, digest: digest, ext: ext, force: force)
                    end
                  when 'asdf'
                    break unless @asdf

                    case flag
                    when :set
                      format_desc action, flag, 'version,opts*=u|home,p|parent'
                      task flag, [:version] do |_, args|
                        args = if (version = args.version)
                                 args.extras
                               else
                                 version, opts = choice_index('Select a version',
                                                              @asdf[1].children
                                                                      .map(&:basename)
                                                                      .sort { |a, b| b <=> a }
                                                                      .push('latest', 'system'),
                                                              accept: [['Confirm?', false, true]],
                                                              values: ['Options'])
                                 OptionPartition.strip(opts)
                               end
                        asdf(flag, args, version: version)
                      end
                    else
                      format_desc(action, flag, flag == :exec ? 'command' : nil)
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
          @withargs = kwargs.empty? ? nil : kwargs
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
            parent = self
            proj = nil
            name = case name
                   when String, Symbol
                     name.to_s
                   else
                     path.basename
                   end
            workspace.add(path, name, **kwargs) do
              __send__ :parent_set, parent
              proj = self
              instance_eval(&blk) if block_given?
            end
            @children << proj
          end
          self
        end

        def chain(*args, **kwargs)
          workspace.chain(*args, project: self, **kwargs)
          self
        end

        def inject(obj, *args, **kwargs, &blk)
          if enabled?
            out = obj.link(self, *args, **kwargs, &blk) if obj.respond_to?(:link)
            if !out
              print_error('link not compatible', subject: obj, hint: name)
            elsif out.respond_to?(:build)
              out.build
            end
          end
          self
        end

        def build(*args, sync: invoked_sync?('build'), from: :run, **)
          banner = verbose
          if args.empty?
            return unless from == :run

            banner = verbosetype > 1 if from_base?('build')
            run_b(@run, sync: sync, from: from, banner: banner) if series?(@run)
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
              case opts
              when Hash
                cmd = Array(cmd).push(flags)
                                .concat(append_hash(opts, target: [], build: true))
                                .compact
                                .join(' ')
              when Enumerable
                cmd = Array(cmd).concat(opts.to_a)
                cmd.map! { |val| "#{val} #{flags}" } if flags
                cmd = cmd.join(' && ')
              else
                cmd = [cmd, flags, opts].compact.join(' ') if opts || flags
              end
            else
              return unless (opts || extra) && respond_to?(:compose)

              cmd = compose(as_get(opts, from), flags, script: true, args: extra, from: from)
              from = :script if from == :run && script?
            end
          end
          run(cmd, var, sync: sync, from: from, banner: banner)
        end

        def depend(*, sync: invoked_sync?('depend'), **)
          run_b(@depend, sync: sync, from: :depend)
        end

        def prereqs(*, sync: invoked_sync?('prereqs'), **)
          on :first, :prereqs
          graph_deps.flatten(1).sort.each do |proj|
            next if @@graph[:_].include?(proj)

            if (val = ENV["PREREQS_#{proj.instance_variable_get(:@envname)}"] || ENV["PREREQS_#{proj.ref.upcase}"])
              split_escape(val).each do |meth|
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
          run_b(@doc, sync: sync, banner: from_base?('doc') ? verbose? : verbosetype > 0, from: :doc)
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
              temp = @clean
              @clean = val
              clean(*args, sync: sync, pass: true, **kwargs)
              @clean = temp
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
                  rm_rf(entry, verbose: verbosetype > 0)
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

        def graph(start = [], tasks = nil, *, sync: invoked_sync?('graph'), pass: [], out: nil, **)
          if (val = env('GRAPH', strict: true))
            tasks ||= []
            split_escape(val).each do |task|
              if ref?(task.to_sym) && (script = workspace.script_get(:graph, ref: task.to_sym))
                tasks.concat(script[:graph])
              else
                tasks << task
              end
            end
          end
          pass.concat(split_escape(val)) if (val = env('GRAPH', suffix: 'PASS'))
          start, neg = start.partition { |name| !name.start_with?('-') }
          data = graph_collect(self, start, pass: neg.map! { |name| name[1..-1] })
          unless out
            data[name] << self
            on :first, :graph
          end
          ret = graph_branch(self, data, tasks, out, sync: sync, pass: pass)
        rescue StandardError => e
          on_error(e, :graph, exception: true)
        else
          if out
            [out, ret]
          else
            on :last, :graph
          end
        end

        def unpack(target, file = nil, uri: nil, sync: true, digest: nil, ext: nil, force: false, depth: 1, headers: {},
                   verbose: self.verbose, from: :unpack)
          if !target.exist?
            target.mkpath
          elsif !target.directory?
            raise_error('invalid location', hint: target)
          elsif !file && !target.empty?
            raise_error('directory not empty', hint: target) unless force || env('UNPACK_FORCE')
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
                     raise_error "invalid checksum: #{digest}"
                   end
          end
          if (val = env('HEADERS')) && (val = parse_json(val, hint: "HEADERS_#{@envname}"))
            headers = headers.is_a?(Hash) ? headers.merge(val) : val
          end
          if file
            ext ||= File.extname(file)[1..-1]
          else
            require 'open-uri'
            data = nil
            (uri = Array(uri)).each_with_index do |url, index|
              URI.open(url, headers) do |f|
                data = f.read
                if algo && algo.hexdigest(data) != digest
                  data = nil
                  raise_error("checksum failed: #{digest}", hint: url) if index == uri.size - 1
                end
                next if ext && index == 0

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
              raise_error("no content#{data ? ' type' : ''}", hint: uri)
            end
          end
          ext = ext.downcase
          if (val = env("#{%w[zip 7z gem].include?(ext) ? ext.upcase : 'TAR'}_DEPTH", ignore: false))
            depth = val.to_i
          end
          begin
            unless file
              if ext == 'gem'
                dir = Dir.mktmpdir
                file = File.new(File.join(dir, File.basename(uri)), 'w')
              else
                require 'tempfile'
                file = Tempfile.new("#{name}-")
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
            when 'tar', 'tgz', 'txz', 'tar.gz', 'tar.xz', 'gz', 'xz'
              flags = +(verbose ? 'v' : '')
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

          cmd = session 'asdf', flag
          name = @asdf.first
          legacy = @@asdf[1] == 15
          case flag
          when :set
            u = has_value?(opts, %w[u home])
            cmd << if legacy
                     cmd.delete(flag)
                     u ? 'global' : 'local'
                   elsif has_value?(opts, %w[p parent])
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
          end
          print_success if success?(run(from: :"asdf:#{flag}"), flag == :set)
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
          data = @events[name.to_sym]
          items = if override
                    data[key.to_sym] = []
                  else
                    data[key.to_sym] ||= []
                  end
          items << [block_given? ? [blk] + args : args, kwargs]
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
            log&.warn "series: @#{key} (invalid)"
          end
          self
        end

        def run(cmd = @session, var = nil, exception: self.exception, sync: true, from: nil, banner: true, chdir: path,
                interactive: nil, hint: nil, series: true, **)
          unless cmd
            print_error('no command session started', subject: project, hint: from, pass: true)
            return
          end
          cmd = cmd.target if cmd.is_a?(OptionPartition)
          if interactive && (!@session || !option('y'))
            title, y = case interactive
                       when Array
                         interactive
                       when String
                         [interactive, 'N']
                       else
                         ['Run', 'Y']
                       end
            exit 1 unless confirm("#{title}? [#{sub_style(cmd.to_s, styles: theme[:inline])}]", y)
          end
          cmd = session_done cmd
          log&.info cmd
          on :first, from
          begin
            if cmd.start_with?(/[^:]+:[^:]/) && workspace.task_defined?(cmd)
              log&.warn "ENV discarded: #{var}" if var
              task_invoke(cmd, exception: exception, warning: warning?)
            else
              print_item(format_banner(hint ? "#{cmd} (#{hint})" : cmd, banner: banner), series: series) if sync
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
            val = case args.size
                  when 0
                    nil
                  when 1
                    args.first
                  else
                    args
                  end
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
            log&.warn "variable_set: @#{key} (private)"
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

        def log
          return @log unless @log.is_a?(Array)

          @log = Logger.new(enabled? ? @log.first : nil, **@log.last)
        end

        def allref(&blk)
          @ref.reverse_each(&blk)
        end

        def basepath(*args)
          path.join(*args)
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

        def inspect
          "#<#{self.class}: #{name} => #{self}>"
        end

        def to_s
          path.to_s
        end

        def to_sym
          name.to_sym
        end

        private

        def puts(*args, **kwargs)
          log_console(*args, pipe: kwargs[:pipe] || pipe)
        end

        def run_s(*cmd, env: nil, sync: true, from: nil, banner: verbose != false, **kwargs)
          on :first, from
          begin
            cmd.flatten.each { |val| run(val, env, sync: sync, banner: banner, **kwargs) }
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
            obj.call
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

        def graph_branch(target, data, tasks = nil, out = nil, sync: true, pass: [], done: [], depth: 0,
                         single: false, last: false, context: nil)
          tag = ->(proj) { "#{proj.name}#{SEM_VER.match?(proj.version) ? "@#{proj.version}" : ''}" }
          script = ->(proj) { workspace.script_get(:graph, group: proj.group, ref: proj.allref)&.fetch(:graph, nil) }
          check = ->(deps) { deps.reject { |val| done.include?(val) } }
          dedupe = lambda do |name|
            next [] unless (ret = data[name])

            ret.dup.each do |proj|
              next if proj.name == name

              data[proj.name]&.each { |dep| ret.delete(dep) if ret.include?(dep) }
            end
            ret
          end
          start = target.name
          if depth == 0
            items = check.call(dedupe.call(start))
            single = items.size == 1
          else
            items = check.call(data[start])
          end
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
                     "#{single ? ' ' : a}#{'   ' * (depth - 1)}#{last ? d : c}#{b * 3}#{items.empty? ? b : e} #{f}"
                   end
          end
          items.each_with_index do |proj, i|
            next if done.include?(proj)

            t = dedupe.call(proj.name)
            j = if out
                  if i == items.size - 1 || check.call(post = items[(i + 1)..-1]).empty?
                    true
                  elsif !t.empty? && depth > 0
                    post.reject { |pr| t.include?(pr) }.empty?
                  end
                end
            unless start == proj.name || (none = check.call(t).empty?)
              graph_branch(proj, data, tasks, out, sync: sync, pass: pass, done: done, depth: depth.succ,
                                                   single: single, last: j == true, context: target)
            end
            if !out
              (tasks || (subtasks = script.call(proj)) || (dev? ? %w[build copy] : %w[depend build])).each do |meth|
                next if pass.include?(meth)

                if workspace.task_defined?(cmd = task_join(proj.name, meth))
                  if ENV.key?(key = "BANNER_#{proj.name.upcase}")
                    key = nil
                  else
                    ENV[key] = '0'
                  end
                  run(cmd, sync: false, banner: false)
                  ENV.delete(key) if key
                elsif proj.has?(meth, tasks || subtasks ? nil : workspace.baseref)
                  proj.__send__(meth.to_sym, sync: sync)
                end
              end
            elsif none
              a, b, c, d = ARG[:GRAPH]
              out << if depth == 0
                       "#{i == items.size - 1 ? d : c}#{b * 4} #{tag.call(proj)}"
                     else
                       s = ''.dup
                       k = 0
                       final = data.keys.last
                       while k < depth
                         indent = k > 0 ? ((last && !j) || (j && k == depth - 1) || single) : j && last && depth == 1
                         s += "#{indent || (last && data[final].last == context) ? ' ' : a}   "
                         k += 1
                       end
                       s + "#{j ? d : c}#{b * 3} #{tag.call(proj)}"
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
              next unless obj.enabled?

              items = [obj]
            else
              items = workspace.find(group: val, ref: val.to_sym)
            end
            items.each do |proj|
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
          a = "#{key}_#{@envname}"
          ret = if suffix
                  ENV.fetch("#{a}_#{suffix}", '')
                elsif strict
                  ENV[a].to_s
                else
                  ignore = ['0'].freeze if ignore.nil?
                  ENV[a] || ENV.fetch(key, '')
                end
          return ret == equals.to_s unless equals.nil?

          ret.empty? || (ignore && Array(ignore).any? { |val| ret == val.to_s }) ? default : ret
        end

        def session(*cmd, prefix: cmd.first, main: true, path: true, options: true)
          prefix = stripext prefix.to_s
          if path && (val = shell_bin(prefix))
            cmd[0] = shell_quote(val, force: false)
          end
          ret = JoinSet.new(cmd.flatten(1))
          if options && (val = env("#{prefix.upcase}_OPTIONS"))
            split_escape(val).each { |opt| ret.last(fill_option(opt), /\A(--?[^\[\]=\s-][^\[\]=\s]*)[=\s].+\z/m) }
          end
          main ? @session = ret : ret
        end

        def session_delete(*args, target: @session)
          OptionPartition.delete(target, *args)
        end

        def session_output(*cmd, **kwargs)
          session(*cmd, main: false, options: false, **kwargs)
        end

        def session_done(cmd)
          return cmd unless cmd.respond_to?(:done)

          raise_error('no args added', hint: cmd.first) unless cmd.size > 1
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
            next unless (ret = env(env_key(stripext(prefix), val), **kwargs))

            return block_given? ? yield(ret) : ret
          end
          nil
        end

        def option_clear(opts, target: @session, **kwargs)
          return unless target

          OptionPartition.clear(target, opts, styles: theme[:inline], **kwargs)
        end

        def print_success(*)
          puts 'Success'
        end

        def print_error(*args, loglevel: Logger::WARN, **kwargs)
          return unless warning?

          warn log_message(loglevel, *args, **kwargs)
        end

        def print_item(*val, series: true)
          puts unless printfirst?
          printsucc if series
          puts val unless val.empty? || (val.size == 1 && val.first.nil?)
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
          (lines << sub_style(ARG[:BORDER][1] * n, styles: border)).join("\n")
        end

        def print_footer(*lines, sub: nil, reverse: false, right: false, border: borderstyle, **)
          n = line_width lines
          lines.map! do |val|
            s = right ? val.rjust(n) : val.ljust(n)
            sub&.each { |h| s = sub_style(s, **h) }
            s
          end
          ret = [sub_style(ARG[:BORDER][1] * n, styles: border)].concat(lines)
          ret.reverse! if reverse
          ret.join("\n")
        end

        def print_status(*args, from: nil, **kwargs)
          return if stdin?

          case from
          when :outdated
            out = print_footer("major #{args[0]} / minor #{args[1]} / patch #{args[2]}", right: true).split("\n")
            out[1] = sub_style(out[1], pat: /^( +major )(\d+)(.+)$/, styles: theme[:major], index: 2)
            out[1] = sub_style(out[1], pat: /^(.+)(minor )(\d+)(.+)$/, styles: theme[:active], index: 3)
            puts out
          when :completed
            if verbose && kwargs[:start]
              msg = sub_style('completed', styles: theme[:active])
              puts log_message(Logger::INFO, *args, msg, subject: kwargs[:subject],
                                                         hint: time_format(time_epoch - kwargs[:start]))
            end
          end
        end

        def format_desc(action, flag, opts = nil, **kwargs)
          return unless TASK_METADATA

          workspace.format_desc([@desc, action, flag].compact, opts, **kwargs)
        end

        def format_banner(cmd, banner: true)
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
                path = $3 || $2 || $1
                name = stripext path
                cmd = cmd.sub(path, data.command == 0 ? name : name.upcase)
              end
              out << cmd
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

        def format_list(items, cmd, type, grep: [], from: nil, each: nil)
          reg = grep.map { |val| Regexp.new(val) }
          out = []
          unless items.empty?
            pad = items.size.to_s.size
            items.each_with_index do |val, i|
              next unless matchany?(val.first, reg)

              out << ('%*d. %s' % [pad, i.succ, each ? each.call(val) : val.first])
            end
          end
          sub = [headerstyle]
          if out.empty?
            out = ["No #{type} were found:", '']
            unless grep.empty?
              i = 0
              out.concat(grep.map { |s| "#{i += 1}. #{s}" })
              out << ''
            end
            if from
              out << (from = from.to_s)
              pat = /\A(#{Regexp.escape(from)})(.*)\z/
            end
          else
            pat = /\A(\s*\d+\.)(.+)\z/
            unless grep.empty?
              footer = "#{out.size} found "
              sub << { pat: /\A(\d+)( .+)\z/, styles: theme[:inline] }
            end
          end
          sub << { pat: pat, styles: theme[:active] } if pat
          emphasize(out, title: task_join(name, cmd), border: borderstyle, sub: sub, footer: footer, right: true)
        end

        def empty_status(msg, title, obj, always: false)
          "#{msg}#{!always && (!obj || obj == 0 || obj.to_s.empty?) ? '' : message(hint: message(title, obj.to_s))}"
        end

        def append_repeat(flag, opts, target: @session, **kwargs)
          opts.each { |val| target << shell_option(flag, val, **kwargs) }
        end

        def append_hash(data, target: @session || [], build: false)
          if build && (type = env('BUILD', suffix: 'TYPE') || ENV['BUILD_TYPE'])
            type = "__#{type}__"
            if (extra = data[type] || data[type.to_sym]).is_a?(Hash)
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
            when Array
              append_repeat(key, val, target: target)
            when Numeric
              target << basic_option(key, val)
            when FalseClass
              target << shell_option(key).sub(/^--(?!no-)/, '--no-')
            when Pathname
              target << shell_option(key, val, escape: false)
            else
              target << shell_option(key, val.is_a?(String) ? val : nil)
            end
          end
          target
        end

        def append_any(val, target: @session, build: false, delim: false)
          return unless val

          if delim && !target.include?('--')
            target << '--'
          else
            delim = false
          end
          val = shell_split(val) if val.is_a?(String)
          case val
          when Hash
            append_hash(val, target: target, build: build)
          when Enumerable
            if target.is_a?(Array)
              target.concat(val.to_a)
            else
              target.merge(val.to_a)
            end
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
                               shell_option(opt, equals ? val : nil, quote: quote, escape: escape, force: force)
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
            ret << shell_option(flag, equals ? val : nil, escape: escape, quote: quote, force: force)
          end
          ret.each { |val| target << val } unless ret.empty?
          ret
        end

        def append_nocolor(target: @session)
          target << '--no-color' if !ARG[:COLOR] || stdin? || option('color', target: target, equals: '0')
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
          raise_error("invalid JSON #{kind.name}", val, hint: hint) if kind && !ret.is_a?(kind)
        rescue StandardError => e
          log&.warn e
          print_error(e, subject: name)
        else
          ret
        end

        def param_guard(action, flag, args:, key: nil, pat: nil, values: nil)
          if args && key
            val = args.fetch(key, nil)
            return val unless val.nil? || (pat && !val.match?(pat)) || (values && !values.include?(val))

            @session = nil
            raise_error(action, "#{flag}[#{key}]", hint: val.nil? ? 'missing' : 'invalid')
          elsif args.is_a?(Array) && args.empty?
            @session = nil
            raise_error(action, "#{flag}+", hint: 'empty')
          end
          args
        end

        def confirm_outdated(pkg, ver, rev, cur = nil, lock: false, col1: 0)
          a = sub_style(case rev
                        when 1
                          'MAJOR'
                        when 2
                          'MINOR'
                        else
                          'PATCH'
                        end, styles: (rev == 1 && theme[:major]) || theme[:header])
          b = sub_style(pkg.ljust(col1), styles: theme[:inline])
          c = lock ? sub_style((cur || 'locked').rjust(7), styles: color(:red)) : cur&.rjust(7)
          d = rev == 1 || lock ? 'N' : 'Y'
          confirm "#{a}: #{b}#{c} #{sub_style(ver.rjust(col1 > 0 ? 10 : 0), styles: theme[:inline])}   ", d
        end

        def choice_index(msg, list, values: nil, accept: nil, series: false, trim: nil, column: nil, multiple: false,
                         force: true, **kwargs)
          puts if !series && !printfirst?
          msg = "#{msg} (optional)" unless force
          unless (ret = choice(msg, list, multiple: multiple, force: force, **kwargs)) && !ret.empty?
            exit 1 if force
            return
          end
          ret = multiple ? ret.map! { |val| val.sub(trim, '') } : ret.sub(trim, '') if trim
          if column
            a, b = Array(column)
            ret = Array(ret).map! { |val| val[a, b || 1] }
            ret = ret.first unless multiple
          end
          if accept
            hint = Array(ret).map { |val| sub_style(val, styles: theme[:inline]) }.join(', ')
            accept = Array(accept).map { |val| Array(val) }
            ret = Array(ret) if accept.any? { |val| val[1] == true }
            loop do
              item = accept.first
              c = confirm("#{item[0]}#{hint ? " [#{hint}]" : ''}", item[2] ? 'Y' : 'N', timeout: 60)
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
              ret << (val.empty? ? nil : val)
            end
          end
          printsucc unless series
          ret
        end

        def command_args(args, min: 0, force: false, **kwargs)
          return if args.size > min || option('i', 'interactive', **kwargs, equals: '0')

          readline('Enter arguments', force: force)
        end

        def block_args(val = nil, &blk)
          if (ret = instance_eval(&blk)).nil?
            val
          else
            Array(ret)
          end
        end

        def runenv
          nil
        end

        def command(*args)
          return args.join(' && ') unless workspace.powershell?

          "#{shell_bin('powershell.exe')} -Command \"& {#{args.join(' ; ')}}\""
        end

        def relativepath(*list, all: false)
          return [] if list.empty?

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
            if val.is_a?(Regexp)
              val
            else
              val = ".*#{val}" if prefix && !val.sub!(/\A(\^|\\A)/, '')
              Regexp.new("#{prefix}#{val == '*' ? '.+' : val}")
            end
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
          val.scan(SEM_VER).first.yield_self { |data| fill ? semver(data) : data }
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
          a.each_with_index do |c, index|
            next if c == (d = b[index])

            return c.to_i < d.to_i ? 1 : -1
          end
          0
        end

        def semgte?(val, other)
          semcmp(val, other) != 1
        end

        def indexitem(val)
          [$1.to_i, $2 && $2[1..-1]] if val =~ /\A[=^#{indexchar}](\d+)(:.+)?\z/
        end

        def indexerror(val, list = nil)
          raise_error("requested index #{val}", hint: list && "of #{list.size}")
        end

        def indexchar
          workspace.windows? ? '=' : '^'
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

        def epochtime
          Time.now.strftime('%s%L').to_i
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

        def on_error(err, from, exception: self.exception, pass: false, dryrun: false)
          log&.error err
          unless dryrun
            ret = on :error, from, err
            raise err if exception && ret != true
          end
          print_error(err, pass: pass) unless ret
        end

        def pwd_set(pass: false, dryrun: false, from: nil)
          return yield if (path.to_s == Dir.pwd || pass == true) && (workspace.mri? || !workspace.windows?)

          pwd = Dir.pwd
          Dir.chdir(path)
          yield
        rescue StandardError => e
          on_error(e, from, dryrun: dryrun)
        ensure
          Dir.chdir(pwd) if pwd
        end

        def run_set(cmd, val = nil, opts: nil, **)
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
          case cmd
          when Array
            @output = if cmd.all? { |data| data.is_a?(Hash) }
                        noopt = false
                        noenv = false
                        cmd.map { |data| parse.call(data) }
                      else
                        cmd.dup
                      end
            return
          when Hash
            @output = parse.call(data)
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

        def script_set(cmd, prod: nil, args: nil, **)
          return if @output[1] == false && @output[0].nil?

          @output[0] = nil
          @output[1] = if @global && cmd.is_a?(Array)
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
                    dir = @@asdf[0].join('installs', val)
                    [val, dir] if dir.exist? && !dir.empty?
                  end
        end

        def theme_set(common)
          @theme = if !verbose
                     {}
                   elsif common
                     workspace.theme
                   else
                     __get__(:theme)[:project][to_sym] ||= {}
                   end
        end

        def dependfile_set(list)
          @dependindex = list.index { |file| basepath(file).exist? }.tap do |index|
            @dependfile = basepath(list[index || 0])
          end
        end

        def as_get(val, from)
          (@global && @as[from][val]) || val
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
              next if (items = @children.select { |item| item.task_include?(key) }).empty?

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
          if val.directory? && !val.empty?
            true
          else
            log&.warn "directory \"#{val}\" (#{val.directory? ? 'empty' : 'not found'})"
            false
          end
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
          return false if workspace.series.chain?(val = task_join(name, action))
          return true if task_invoked?(val) && (!task_invoked?(ac) || !workspace.task_defined?(ac, 'sync'))

          workspace.series.name_get(action).yield_self { |name| name != action && invoked_sync?(name) }
        end

        def success?(ret, display = true)
          ret == true && display && stdout? && banner?
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

        def warning?
          workspace.warning
        end

        def has_value?(data, other)
          return false unless data.is_a?(Enumerable)

          other.is_a?(Enumerable) ? other.any? { |obj,| data.include?(obj) } : data.include?(other)
        end

        def variables
          VAR_SET
        end

        def blocks
          BLK_SET
        end

        def hashobj
          Workspace::Support.hashobj
        end

        def hashlist
          Workspace::Support.hashlist
        end

        def hashdup
          Workspace::Support.hashdup
        end

        def borderstyle
          workspace.banner_get(*@ref, group: group)&.border || theme[:border]
        end

        def headerstyle
          { pat: /^(\S+)(\s+)$/, styles: theme[:header] }
        end

        def scriptargs
          { target: script? ? @output[1] : @output[0], ref: ref, group: group, global: @global }
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
        elsif name.is_a?(Enumerable)
          data = name.to_h
        elsif name.is_a?(String) && name.match?(GIT_PROTO)
          base = name
          @project.each_value { |proj| repo << proj if !proj.parent && check.call(proj) }
        else
          warn log_message(Logger::WARN, name, subject: 'git', hint: 'invalid') if warning
          return self
        end
        if base
          base = base.match?(GIT_PROTO) ? "#{base.chomp('/')}/" : @root + base
          repo.each do |target|
            if target.is_a?(Project::Git)
              data[target.localname] = target.project
            else
              data[target.to_s] = nil
            end
          end
        end
        data.each do |key, val|
          if val.is_a?(Hash)
            uri = val.fetch(:uri, '')
            opts = val.fetch(:options, {})
          else
            uri = val.is_a?(String) ? val : key.to_s
            opts = options
          end
          unless uri.match?(GIT_PROTO) || Pathname.new(uri).absolute?
            if uri.start_with?('.')
              uri = @root + uri
            elsif base
              uri = base + uri
            else
              next
            end
          end
          key = task_name key
          GIT_REPO[main][key] = [uri.to_s, opts]
          @kind[key] << Project::Git
        end
        if cache == true
          revbuild
        elsif cache
          revbuild(file: cache)
        end
        self
      end

      def revbuild(file: nil)
        @revfile = @home.join(file || "#{@main}.revb")
        @revdoc = JSON.parse(@revfile.read) if @revfile.exist?
      rescue StandardError => e
        @revfile = nil
        warn log_message(Logger::WARN, e, pass: true)
        self
      else
        @revdoc = {} unless @revdoc.is_a?(Hash)
        self
      end

      def git_repo(name)
        (ret = GIT_REPO[main]) && ret[name]
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
        warn log_message(Logger::WARN, e, pass: true) if warning
      ensure
        @revlock = false
      end

      def git_clone?(path, name = nil)
        return false if name && !git_repo(name)

        !path.exist? || path.empty?
      end

      private

      def rev_timenow
        Time.now.utc.strftime('%s%L').to_i
      end
    end
    Application.include Git

    module Project
      class Git < Base
        OPT_GIT = {
          common: %w[c=q bare glob-pathspecs icase-pathspecs literal-pathspecs no-optional-locks no-pager
                     no-replace-objects noglob-pathspecs paginate attr-source=b config-env=q exec-path=p
                     namespace=p].freeze,
          add: %w[A|all e|edit f|force ignore-errors ignore-missing ignore-removal i|interactive no-all
                  no-ignore-removal n|dry-run p|patch pathspec-file-nul renormalize sparse u|update v|verbose
                  chmod=b pathspec-from-file=p].freeze,
          branch: %w[a|all create-reflog i|ignore-case omit-empty q|quiet r|remotes v|verbose abbrev=i color=b
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
            pull: %w[4 6 n t a|append atomic dry-run f|force k|keep negotiate-only prefetch p|prune q|quiet
                     set-upstream unshallow update-shallow v|verbose deepen=i depth=i j|jobs=i negotiation-tip=q
                     recurse-submodules=v refmap=q o|server-option=q shallow-exclude=b shallow-since=v
                     upload-pack=q].freeze
          }.freeze,
          git: {
            add: %w[N|intent-to-add refresh].freeze,
            blame: %w[b c l s t w C=im? L=q M=im? S=p color-by-age color-lines first-parent incremental line-porcelain
                      p|porcelain root score-debug f|show-name e|show-email n|show-number show-stats abbrev=i
                      contents=p date=q encoding=b ignore-rev=b ignore-revs-file=p reverse=q].freeze,
            clean: %w[d x X f|force n|dry-run i|interactive q|quiet e|exclude=q].freeze,
            mv: %w[k f|force n|dry-run v|verbose].freeze,
            revert: %w[e S=bm? abort continue n|no-commit quit reference skip cleanup=b gpg-sign=b? m|mainline=i
                       s|signoff strategy=b X|strategy-option=b].freeze,
            rm: %w[r cached f|force n|dry-run ignore-unmatch pathspec-file-nul q|quiet sparse v|verbose
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
            diff: %w[p R u z B=bm? C=bm? l=im G=qm I=qm M=bm? O=qm S=qm U=im binary check compact-summary cumulative
                     find-copies-harder full-index W|function-context w|ignore-all-space ignore-blank-lines
                     ignore-cr-at-eol ignore-space-at-eol b|ignore-space-change D|irreversible-delete graph
                     ita-invisible-in-index minimal name-only name-status no-color-moved-ws no-prefix no-renames numstat
                     patch-with-raw patch-with-stat patience pickaxe-all pickaxe-regex raw shortstat summary a|text
                     abbrev=i? anchored=q break-rewrites=b? color=b color-moved=b color-moved-ws=b color-words=q?
                     diff-algorithm=b diff-filter=e? X|dirstat=b? dirstat-by-file=b? dst-prefix=q find-copies=i?
                     find-object=b find-renames=b? ignore-matching-lines=q ignore-submodules=b? inter-hunk-context=i
                     line-prefix=q output=p output-indicator-context=q output-indicator-new=q output-indicator-old=q
                     relative=p rotate-to=p skip-to=p src-prefix=q stat=b? stat-count=i stat-width=i stat-name-width=i
                     submodule=b? unified=i word-diff=b? word-diff-regex=q ws-error-highlight=b].freeze
          }.freeze,
          ls_files: %w[f t v z debug deduplicate directory eol error-unmatch exclude-standard full-name i|ignored
                       k|killed no-empty-directory recurse-submodules sparse s|stage u|unmerged abbrev=i x|exclude=q
                       X|exclude-from=p exclude-per-directory=p format=q with-tree=q].freeze,
          ls_remote: %w[exit-code get-url q|quiet symref o|server-option=q sort=q upload-pack=q].freeze,
          merge: %w[e n S=bm? allow-unrelated-histories ff-only m=q q|quiet v|verbose cleanup=b F|file=p gpg-sign=b?
                    into-name=b log=i s|strategy=b X|strategy-option=b].freeze,
          pull: %w[e n S=bm? allow-unrelated-histories ff-only cleanup=b gpg-sign=b? log=i r|rebase=v? s|strategy=b
                   X|strategy-option=b].freeze,
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
            push: %w[a|all u|include-untracked k|keep-index no-keep-index no-include-untracked pathspec-file-nul
                     p|patch S|staged m|message=q pathspec-from-file=p].freeze,
            pop: %w[index].freeze,
            apply: %w[index].freeze
          }.freeze,
          status: %w[z u=bm? b|branch long s|short show-stash v|verbose column=b find-renames=i? ignore-submodules=b?
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
            blame: %w[progress].freeze,
            branch: %w[color color-moved column track].freeze,
            checkout: %w[overwrite-ignore guess overlay progress recurse-submodules track].freeze,
            fetch: {
              base: %w[auto-gc auto-maintenance write-commit-graph write-fetch-head].freeze,
              pull: %w[all ipv4 ipv6 recurse-submodules show-forced-updates tags].freeze
            },
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
          reset: %w[soft mixed hard merge keep recurse-submodules no-recurse-submodules].freeze
        }.freeze
        private_constant :OPT_GIT, :VAL_GIT

        class << self
          include Rake::DSL

          def populate(ws, **)
            return if ws.series.exclude?(:pull, true) || ws.size == 1

            namespace ws.task_name('git') do |ns|
              ws.format_desc(all = ws.task_join(ns.scope.path, 'all'), 'stash|rebase|autostash?,depend?')
              task 'all' do |_, args|
                args = args.to_a
                cmd = if args.include?('stash')
                        ['stash', 'pull']
                      elsif args.include?('rebase')
                        ['rebase']
                      elsif args.include?('autostash')
                        ['autostash']
                      else
                        ['pull']
                      end
                cmd.map! { |val| ws.task_sync(val) }
                cmd << ws.task_sync('depend') if args.include?('depend') && !ws.series.exclude?(:depend, true)
                cmd << ws.task_sync('build')
                Common::Utils.task_invoke(*cmd, **ws.invokeargs)
              end

              ws.series.sync << all
              ws.series.multiple << all
            end
          end

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
          'git' => %i[add blame clean mv revert rm status].freeze,
          'log' => %i[view between contain].freeze,
          'merge' => %i[commit no-commit send].freeze,
          'pull' => %i[origin remote all].freeze,
          'rebase' => %i[branch onto send].freeze,
          'refs' => %i[heads tags remote].freeze,
          'reset' => %i[commit index patch mode undo].freeze,
          'restore' => %i[source staged worktree].freeze,
          'rev' => %i[commit build output].freeze,
          'show' => %i[format oneline textconv].freeze,
          'stash' => %i[push pop apply branch drop clear list all].freeze,
          'submodule' => %i[status update branch url sync].freeze,
          'switch' => %i[branch create detach].freeze,
          'tag' => %i[add sign delete list].freeze
        })

        def initialize(*, **)
          super
          @submodule = basepath('.gitmodules').exist?
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
                        if (remote = args.remote)
                          args = args.extras
                        else
                          remote = choice_remote
                          args = args.to_a
                        end
                        __send__(action, flag, args, remote: remote)
                      end
                    else
                      format_desc(action, flag, 'opts*', after: flag == :all && action == 'pull' ? 'pattern*' : nil)
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
                      format_desc(action, flag, 'pathspec+', before: flag == :add ? 'opts*' : nil)
                      task flag do |_, args|
                        if flag == :fixup
                          ref, squash, pick = choice_commit(accept: [['Auto squash?', true]], reflog: false,
                                                            values: ['Pick [amend|reword]'])
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
                          refs = choice_refs('Choose a tag', 'tags', multiple: true, accept: 'Delete?', series: true)
                          remote = choice_remote
                        end
                        tag(flag, refs: refs, remote: remote)
                      end
                    when :add, :sign
                      format_desc action, flag, 'name,message?,commit?,remote?'
                      task flag, [:name, :message, :commit, :remote] do |_, args|
                        if (name = args.name)
                          message = args.message
                          commit = commithead args.commit
                          remote = args.remote
                        else
                          commit, name, message = choice_commit(values: [['Enter tag name', true], 'Enter message'],
                                                                series: true, reflog: false)
                          remote = choice_remote
                        end
                        ret = tag(flag, refs: [name], message: message, commit: commit, remote: remote)
                        print_success if success?(ret, !remote)
                      end
                    end
                  when 'stash'
                    format_desc(action, flag, 'opts*', after: case flag
                                                              when :push then 'pathspec*,:'
                                                              when :branch then 'name,stash?|:'
                                                              when :clear, :list, :all then nil
                                                              else 'stash?|:'
                                                              end)
                    task flag do |_, args|
                      stash flag, args.to_a
                    end
                  when 'log', 'diff'
                    case flag
                    when :view, :between, :contain
                      view = flag == :view
                      if view && action == 'log'
                        format_desc action, flag, '(^)commit*|:,opts*,ref?,pathspec*'
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
                          if commit1
                            commit2 = commithead param_guard(action, flag, args: args, key: :commit2)
                            args = args.extras
                            range = [commit1, commit2]
                          else
                            range, opts, refs = choice_commit(multiple: view ? true : 2, values: %w[Options Pathspec])
                            range = range.reverse
                            args = OptionPartition.strip(opts)
                            args.concat(refs.shellsplit) if refs
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
                          args.each { |val| index << (commithead(val) || commithash(val) || break) }
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
                      format_desc action, flag, 'path1,path2'
                      task flag, [:path1, :path2] do |_, args|
                        path1 = param_guard(action, flag, args: args, key: :path1)
                        path2 = param_guard(action, flag, args: args, key: :path2)
                        diff(flag, refs: [path1, path2])
                      end
                    end
                  when 'checkout'
                    case flag
                    when :branch
                      format_desc action, flag, 'name,create?=[bB],commit?,detach?=d'
                      task flag, [:name, :create, :commit, :detach] do |_, args|
                        if (branch = args.name)
                          branch = param_guard(action, flag, args: args, key: :name)
                          create = args.create
                          if args.commit == 'd'
                            detach = 'd'
                            commit = nil
                          elsif create == 'd'
                            create = nil
                            commit = nil
                            detach = 'd'
                          elsif create && create.size > 1
                            commit = commithead create
                            create = nil
                            detach = args.commit
                          else
                            detach = args.detach
                            commit = commithead args.commit
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
                          origin, branch = choice_refs('Choose a remote', 'remotes', values: ['Enter branch name'])
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
                                 commit, opts = choice_commit(values: ['Options'])
                                 OptionPartition.strip(opts)
                               end
                        checkout(flag, args, commit: commit)
                      end
                    when :detach
                      format_desc action, flag, 'ref?'
                      task flag, [:commit] do |_, args|
                        commit = commithead args.commit
                        unless commit
                          commit, merge = choice_commit(values: ['Merge? [y/N]'])
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
                      format_desc action, flag, 'name,ref?=HEAD|:'
                      task flag, [:name, :ref] do |_, args|
                        target = param_guard(action, flag, args: args, key: :name)
                        ref = commithead args.ref
                        ref, remote = choice_refs('Choose a remote', 'remotes', accept: [['Push?', true]]) if ref == ':'
                        branch(flag, target: target, ref: ref, remote: remote)
                      end
                    when :track
                      format_desc action, flag, '(^~)upstream?,name?'
                      task flag, [:upstream, :name] do |_, args|
                        if (ref = args.upstream)
                          target = args.name
                          remote = true if ref.delete_prefix!('~')
                        else
                          ref, remote, target = choice_refs('Choose a remote', 'remotes', accept: [['Push?', true]],
                                                                                          values: ['Enter branch name'])
                        end
                        branch(flag, target: target, ref: ref, remote: remote)
                      end
                    when :delete
                      format_desc action, flag, '[^~]name*,:?'
                      task flag do |_, args|
                        refs = args.to_a
                        if refs.empty? || (r = refs.last == ':')
                          accept = ['Delete?']
                          accept << ['Force?', true] unless r
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
                      format_desc action, flag, '(^)name,ref?=HEAD|:'
                      task flag, [:name, :commit] do |_, args|
                        branch = param_guard(action, flag, args: args, key: :name)
                        commit = commithead args.commit
                        commit, track = choice_commit(values: ['Track? [Y/n]'], force: false) if commit == ':'
                        switch(flag, branch: branch, commit: commit, track: track)
                      end
                    when :detach
                      format_desc action, flag, 'ref?=HEAD'
                      task flag, [:commit] do |_, args|
                        commit = commithead(args.commit) || choice_commit(force: false)
                        switch(flag, commit: commit)
                      end
                    when :branch
                      format_desc action, flag, 'name|:,opts*'
                      task flag, [:name] do |_, args|
                        if (branch = args.name)
                          args = args.extras
                          branch = nil if branch == ':'
                        else
                          args = []
                        end
                        switch(flag, args, branch: branch || choice_refs('Choose a branch'))
                      end
                    end
                  when 'reset'
                    case flag
                    when :commit
                      format_desc action, flag, 'ref|:,opts*'
                      task flag, [:commit] do |_, args|
                        commit = commithead args.commit
                        if commit && commit != ':'
                          args = args.extras
                        else
                          commit, mode = choice_commit(values: ['Mode [mixed|soft|hard|N]'])
                          args = args.extras.concat(case mode&.downcase
                                                    when 'h', 'hard' then ['hard']
                                                    when 's', 'soft' then ['soft']
                                                    when 'n', 'N' then ['mixed', 'N']
                                                    else ['mixed']
                                                    end)
                        end
                        print_success if success?(reset(flag, args, commit: commit))
                      end
                    when :index, :undo
                      format_desc(action, flag, flag == :index ? 'opts*,pathspec*' : nil)
                      task flag do |_, args|
                        reset(flag, flag == :index ? args.to_a : [])
                      end
                    when :mode
                      format_desc action, flag, 'mode,ref?=HEAD|:'
                      task flag, [:mode, :ref] do |_, args|
                        mode = param_guard(action, flag, args: args, key: :mode)
                        ref = commithead args.ref
                        ref = choice_commit(reflog: false) if ref == ':'
                        reset(flag, mode: mode, ref: ref)
                      end
                    when :patch
                      format_desc action, flag, 'ref?=HEAD|:,pathspec*'
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
                      format_desc action, flag, 'upstream,branch?=HEAD,opts*'
                      task flag, [:upstream] do |_, args|
                        args = if (upstream = args.upstream)
                                 args.extras
                               else
                                 upstream, opts = choice_refs('Choose upstream branch', values: ['Options'])
                                 OptionPartition.strip(opts)
                               end
                        rebase(flag, args, upstream: upstream)
                      end
                    when :onto
                      format_desc action, flag, 'ref,upstream,branch?=HEAD'
                      task flag, [:commit, :upstream, :branch] do |_, args|
                        commit = commithead args.commit
                        args = if commit
                                 upstream = param_guard(action, flag, args: args, key: :upstream)
                                 branch = args.branch
                                 []
                               else
                                 commit = choice_refs 'Choose "onto" branch'
                                 target, opts = choice_commit(reflog: false, multiple: 2, values: ['Options'])
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
                          branch, opts = choice_refs('Choose a branch', values: ['Options'], accept: accept)
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
                      format_desc action, flag, 'ref?=HEAD,size?'
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
                      format_desc(action, flag, 'opts*,pattern*', after: action == 'files' ? 'pathspec*' : nil)
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
                      format_desc action, flag, 'opts*,pathspec*,:?'
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
                             when :blame then 'file'
                             when :mv then 'source+,destination'
                             when :revert then 'commit+'
                             end
                    format_desc(action, flag, 'opts*', before: before, after: case flag
                                                                              when :add
                                                                                'pathspec*,pattern*'
                                                                              when :clean, :rm, :status
                                                                                'pathspec*'
                                                                              end)
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
              raise_error('head not found', hint: 'for-each-ref') unless cur
              opts << 'ff-only' if opts.empty? && !option('ff-only', equals: '0')
              (heads.dup << cur).each_with_index do |branch, index|
                next unless (index < heads.size && cur != branch) || index == heads.size

                git_spawn 'switch --quiet', force && '--force', shell_quote(branch)
                pull(nil, opts, sync: false, hint: branch) if heads.include?(branch)
              end
              return
            when :autostash
              cmd << '--autostash'
            end
          end
          append_pull(opts, OPT_GIT[:pull] + OPT_GIT[:fetch][:pull],
                      no: OPT_GIT[:no][:pull] + OPT_GIT[:no][:fetch][:pull], remote: remote, flag: flag, from: :pull)
          source(sync: sync, sub: if stdout?
                                    [
                                      { pat: /^(.+)(\|\s+\d+\s+)([^-]*)(-+)(.*)$/, styles: color(:red), index: 4 },
                                      { pat: /^(.+)(\|\s+\d+\s+)(\++)(.*)$/, styles: color(:green), index: 3 }
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
            append_head op.shift
            op.clear(pass: false)
          when :onto
            return unless upstream

            cmd << '--interactive' if option('interactive', 'i')
            cmd << shell_option('onto', commit) if commit
            cmd << upstream
            append_head branch
          else
            unless gitpath('REBASE_HEAD').exist?
              puts log_message(Logger::INFO, name, 'no rebase in progress', hint: command) if stdout?
              return
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
          append_pull(opts, collect_hash(OPT_GIT[:fetch]), no: collect_hash(OPT_GIT[:no][:fetch]),
                                                           remote: remote, flag: flag, from: :fetch)
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
          opts.delete(:'recurse-submodules') || opts.delete(:'no-recurse-submodules') if append_submodules(from: :clone)
          append_hash opts
          cmd << '--quiet' unless verbose
          append_value(data[0], path, delim: true)
          source(sync: sync, banner: sync && !quiet?, multiple: !sync || quiet?)
        end

        def stash(flag = nil, opts = [], sync: invoked_sync?('stash', flag))
          if flag
            if flag == :all
              opts << 'include-untracked'
              flag = :push
            end
            cmd, opts = git_session('stash', flag, opts: opts)
            list = OPT_GIT[:stash][:common] + OPT_GIT[:stash].fetch(flag, [])
            if flag == :list
              list += collect_hash OPT_GIT[:log]
              no = collect_hash OPT_GIT[:no][:log]
            end
            op = OptionPartition.new(opts, list, cmd, project: self, no: no, first: flag == :push ? matchpathspec : nil)
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
                    op << op.shift
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
                op << op.shift
              elsif flag == :branch
                raise_error 'no branch name'
              end
              op.clear
            when :clear
              source(stdout: true) if confirm("Remove #{sub_style('all', styles: theme[:active])} stash entries?", 'N')
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
          source(banner: !quiet?, sync: sync, **threadargs)
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
                      { pat: /^(.)([A-Z?!])(.+)$/, styles: r, index: 2 },
                      { pat: /^([A-Z?!])(.+)$/, styles: g },
                      { pat: /^(\?\?)(.+)$/, styles: r },
                      { pat: /^(## )((?~\.{3}))(\.{3})(.+)$/, styles: [nil, g, nil, r], index: -1 }
                    ]
                  else
                    [pat: /^(\t+)([a-z]+: +.+)$/, styles: r, index: 2]
                  end
          end
          out, banner, from = source(io: true)
          ret = write_lines(out, banner: banner, sub: sub)
          list_result(ret, 'files', from: from, action: 'modified')
        end

        def revbuild(flag = nil, opts = [], sync: nil, **kwargs)
          statusargs = lambda do
            {
              include: relativepath(Array(kwargs[:include]), all: true),
              exclude: relativepath(Array(kwargs[:exclude]), all: true)
            }
          end
          unless workspace.closed
            if @revbuild
              statusargs.call.each { |key, val| @revbuild[key] += val }
            else
              @revbuild = statusargs.call
            end
            return
          end
          sha = git_spawn('rev-parse --verify HEAD').chomp
          return if sha.empty?

          sync = invoked_sync?('revbuild', flag) if sync.nil?
          kwargs = kwargs.key?(:include) || kwargs.key?(:exclude) ? statusargs.call : @revbuild || {}
          case flag
          when :build
            op = OptionPartition.new(opts, %w[ignore-submodules=b? ignored=b? untracked-files=b?], project: self)
            op.clear(append: true)
            args = op.to_a
          else
            args = []
            option('untracked-files', prefix: 'git') { |val| args << basic_option('untracked-files', val) }
            option('ignore-submodules', prefix: 'git') { |val| args << basic_option('ignore-submodules', val) }
            option('ignored', prefix: 'git') { |val| args << basic_option('ignored', val) }
          end
          if (cur = workspace.rev_entry(name)) && cur['revision'] == sha && !env('REVBUILD_FORCE')
            files = status_digest(*args, **kwargs)
            if cur['files'].size == files.size && cur['files'].find { |key, val| files[key] != val }.nil?
              if stdout?
                if (since = workspace.rev_timesince(name, 'build'))
                  puts log_message(Logger::INFO, name, 'no changes', subject: 'revbuild', hint: "#{since} ago")
                else
                  workspace.rev_timeutc(name, 'build')
                end
              end
              return
            end
          end
          start = time_epoch
          build(*@output, sync: sync, from: :'git:revbuild')
        rescue StandardError => e
          print_error(e, pass: true)
        else
          print_status(name, subject: 'revbuild', start: start, from: :completed)
          workspace.rev_write(name, { 'revision' => sha, 'files' => status_digest(*args, **kwargs) },
                              sync: sync, utc: 'build')
        end

        def reset(flag, opts = [], refs: nil, ref: nil, mode: nil, commit: nil)
          cmd, opts = git_session('reset', opts: opts)
          case flag
          when :commit, :index
            op = OptionPartition.new(opts, OPT_GIT[:reset] + VAL_GIT[:reset], cmd,
                                     project: self, no: OPT_GIT[:no][:reset],
                                     first: flag == :index ? matchpathspec : nil)
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
          append_option 'force', 'f', 'merge'
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
            op = OptionPartition.new(opts, OPT_GIT[:checkout], cmd, project: self, no: OPT_GIT[:no][:checkout],
                                                                    first: flag == :path ? matchpathspec : nil)
            if flag == :path
              append_head
              append_pathspec(op.extras, pass: false)
              print_success if success?(source)
              return
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
            cmd << '--force' if option('force', 'f')
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
            list_result(ret, 'tags', from: from, grep: op.extras)
            return
          end
          remote ||= option 'remote'
          source
          git_spawn('push', flag == :delete ? '-d' : nil, remote, *refs.map { |val| shell_quote(val) }) if remote
        end

        def log!(flag, opts = [], range: [], index: [])
          cmd, opts = git_session('log', opts: opts)
          op = OptionPartition.new(opts, collect_hash(OPT_GIT[:log]), cmd, project: self,
                                                                           no: collect_hash(OPT_GIT[:no][:log]),
                                                                           first: matchpathspec)
          case flag
          when :between, :contain
            op.add_quote(range.join(flag == :between ? '..' : '...'))
          else
            op.merge(index)
          end
          append_nocolor
          append_pathspec op.extras
          source(exception: false)
        end

        def diff(flag, opts = [], refs: [], branch: nil, range: [], index: [])
          cmd, opts = git_session('diff', opts: opts)
          op = OptionPartition.new(opts, collect_hash(OPT_GIT[:diff]) + OPT_GIT[:log][:diff], cmd,
                                   project: self, no: OPT_GIT[:no][:log][:diff],
                                   first: flag == :files ? nil : matchpathspec)
          case flag
          when :files, :view, :between, :contain
            op.delete('--cached')
          end
          append_nocolor
          if flag == :files
            op << '--no-index'
            append_pathspec(refs, parent: true)
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
                  raise_error("one commit only: #{index.join(', ')}", hint: 'cached') if index.size > 1
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
          amend = !fixup && flag.to_s.start_with?('amend')
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
            source git_session('commit', basic_option('fixup', "#{pick ? "#{pick}:" : ''}#{ref}"), pathspec)
            source git_output('rebase --autosquash', squash) if squash.is_a?(String)
            return
          end
          message ||= messageopt
          if !message && !amend
            return if pass

            message = readline('Enter message', force: true)
          end
          branch = nil
          origin = nil
          upstream = nil
          cmd, opts = git_session('add', opts: opts)
          op = OptionPartition.new(opts, OPT_GIT[:add], cmd, project: self, first: matchpathspec)
          op << '--verbose' if verbose
          format = '%(if)%(HEAD)%(then)%(refname:short)...%(upstream:short)...%(upstream:track)%(end)'
          git_spawn 'fetch --no-tags --quiet'
          foreachref('heads', format: format).each do |line|
            next if (line = line.chomp).empty?

            branch, origin, hint = line.split('...')
            if hint && !hint.match?(/^\[(\D+0,\D+0)\]$/)
              raise_error('work tree is not usable', hint: hint[1..-2])
            elsif !origin || origin.empty?
              return nil if pass
              break if dryrun?

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
              puts(cached.lines.map! { |val| "cached #{shell_quote(val.chomp)}" })
            end
            source co
            source pu
          elsif banner?
            puts 'Nothing to commit'
          elsif stdout?
            puts log_message(Logger::INFO, name, 'nothing to commit', hint: flag)
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
              raise_error 'no branch/commit' if op.empty?
              append_commit(*op.extras)
            end
          else
            unless gitpath('MERGE_HEAD').exist?
              puts log_message(Logger::INFO, name, 'no merge in progress', hint: command) if stdout?
              return
            end
            return unless VAL_GIT[:merge][:send].include?(command)

            cmd << "--#{command}"
            display = command == 'abort'
          end
          print_success if success?(source, display)
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
            cmd << '--force' if option('force', 'f')
            cmd << shell_quote(target)
            cmd << shell_quote(ref) if ref
          when :track
            raise_error('invalid upstream', hint: ref) unless ref.include?('/')
            if ref.delete_prefix!('^')
              cmd << '--unset-upstream' << shell_quote(ref)
              remote = false
              stdout = true
            else
              cmd << quote_option('set-upstream-to', ref)
              cmd << shell_quote(target) if target
            end
          when :delete
            remote&.each do |val|
              source git_output('push --delete', *val.split('/', 2).map { |s| shell_quote(s) })
            end
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
            s.upcase! if option('force', 'f')
            cmd << s
            refs.compact.each { |val| cmd << shell_quote(val) }
            stdout = true
          when :current
            cmd << '--show-current'
            source(banner: verbosetype > 1, stdout: true)
            return
          when :list
            op = OptionPartition.new(opts, OPT_GIT[:branch], cmd << '--list',
                                     project: self, no: OPT_GIT[:no][:branch], single: /\Av+\z/)
            op.each { |val| op.add_quote(val) }
            out, banner, from = source(io: true)
            print_item banner
            ret = write_lines(out, sub: [
              { pat: /^(\*\s+)(\S+)(.*)$/, styles: color(:green), index: 2 },
              { pat: %r{^(\s*)(remotes/\S+)(.*)$}, styles: color(:red), index: 2 }
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
              ret = write_lines(out, grep: [/^\*\s+#{Regexp.escape(head)}\s/], banner: banner, first: true) do |line|
                next line if stdin?

                data = line.sub(/^\*\s+/, '').split(/\s+/)
                a = sub_style(data[0], styles: theme[:inline])
                b = sub_style(data[1], styles: theme[:extra])
                r = /\A(?:\[((?~\]\s))\]\s)?(.+)\z/m.match(data[2..-1].join(' '))
                if (r1 = r[1]) && r1 =~ /^(.+):(?: ([a-z]+) (\d+),)? ([a-z]+) (\d+)$/
                  write = ->(s1, s2) { "#{s1.capitalize.rjust(7)}: #{sub_style(s2, styles: theme[:warn])}" }
                  r1 = $1
                  r2 = $2 && write.call($2, $3)
                  r3 = write.call($4, $5)
                end
                r1 = nil if r1 == "origin/#{data[0]}"
                [" Branch: #{a + (r1 ? " (#{r1})" : '')}", r2, r3, " Commit: #{b}", "Message: #{r[2]}"]
                  .compact
                  .join("\n")
              end
              on :last, from
            end
            print_error(name, 'no ref found', subject: 'branch', hint: 'head', pass: true) if ret == 0
            return
          end
          return unless success?(source(stdout: stdout))

          if !ref
            print_success if flag == :create
          elsif remote && target
            source git_output('push -u', shell_quote(ref.split('/', 2).first), shell_quote(target))
          end
        end

        def switch(flag, opts = [], branch: nil, commit: nil, track: nil)
          cmd, opts = git_session('switch', opts: opts)
          cmd << '--force' if option('force', 'f')
          if flag == :branch
            op = OptionPartition.new(opts, OPT_GIT[:switch], cmd, project: self, no: OPT_GIT[:no][:switch])
            op.add_quote(branch)
          else
            case flag
            when :create
              cmd << quote_option(branch.delete_prefix!('^') ? 'C' : 'c', branch)
              cmd << case (track ||= option('track', ignore: false))
                     when 'n', 'N', '0', 'false'
                       '--no-track'
                     when 'y', 'Y', '1', 'true'
                       '--track'
                     when 'direct', 'inherit'
                       basic_option 'track', track
                     end
            when :detach
              cmd << "--#{flag}"
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
            op << '--recursive' if option('recursive', 'r')
            op.splice(path: true)
          end
          print_success if success?(source, flag == :branch)
        end

        def restore(flag, opts = [], commit: nil, files: nil)
          cmd, opts = git_session('restore', shell_option(flag, commit, escape: false, force: false), opts: opts)
          op = OptionPartition.new(opts, OPT_GIT[:restore], cmd, project: self, no: OPT_GIT[:no][:restore],
                                                                 first: matchpathspec)
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
          when /(?:^t?format:|%)/
            cmd << quote_option('pretty', format)
          else
            opts << format if format
          end
          op = OptionPartition.new(opts, OPT_GIT[:show] + OPT_GIT[:diff][:show] + OPT_GIT[:log][:diff], cmd,
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
            op = OptionPartition.new(opts, OPT_GIT[:rev_parse], cmd, project: self, no: OPT_GIT[:no][:rev_parse],
                                                                     args: args)
            op.append(escape: args)
          end
          source(banner: verbosetype > 1)
        end

        def ls_remote(flag, opts = [], remote: nil)
          cmd, opts = git_session('ls-remote', '--refs', opts: opts)
          cmd << "--#{flag}" unless flag == :remote
          op = OptionPartition.new(opts, OPT_GIT[:ls_remote], cmd, project: self)
          op.add_quote(remote) if remote
          out, banner, from = source(io: true)
          print_item banner
          ret = write_lines(out, grep: op.extras, prefix: "refs/#{flag}/")
          list_result(ret, flag.to_s, from: from, grep: op.extras)
        end

        def ls_files(flag, opts = [])
          cmd, opts = git_session('ls-files', "--#{flag}", opts: opts)
          op = OptionPartition.new(opts, OPT_GIT[:ls_files], cmd, project: self)
          op.splice(path: true, pattern: true)
          out, banner, from = source(io: true)
          print_item banner
          ret = write_lines(out, grep: op.extras)
          list_result(ret, 'files', from: from, grep: op.extras)
        end

        def git(flag, opts = [])
          cmd, opts = git_session(flag, opts: opts)
          op = OptionPartition.new(opts, OPT_GIT[:git].fetch(flag, []) + OPT_GIT.fetch(flag, []), cmd,
                                   project: self, no: OPT_GIT[:no][flag], first: case flag
                                                                                 when :blame, :revert
                                                                                   nil
                                                                                 else matchpathspec
                                                                                 end)
          case flag
          when :blame
            raise_error 'no file found' unless (n = op.index { |s| basepath(s).file? })
            op.delim << shell_quote(basepath(op.delete_at(n)))
            op.clear
          when :revert
            if VAL_GIT[:rebase][:send].any? { |val| op.arg?(val) }
              op.clear
            elsif op.empty?
              raise_error 'no commit given'
            else
              append_commit(*op.extras)
            end
          when :add
            if flag == :add && !op.arg?('pathspec-from-file')
              grep, list = op.partition { |val| OptionPartition.pattern?(val) }
              unless grep.empty? && !list.empty?
                grep.map! { |val| Regexp.new(val[1..-2]) }
                files = []
                status_data.each do |a, b|
                  next if b.strip.empty? || (!grep.empty? && grep.none? { |pat| pat.match?(a) })

                  files << "#{sub_style(b, styles: color(:red))} #{a}"
                end
                unless files.empty?
                  files = choice_index('Select files', files, multiple: true, trim: /^\S+\s/,
                                                              accept: [['Add?', false, true]])
                end
                op.swap(list + files)
              end
            end
            return source(git_session('status -s'), banner: false) unless append_pathspec(op.extras)

            print_success if success?(source, flag == :add && !op.arg?('verbose'))
            return
          when :mv
            refs = projectmap op.extras
            raise_error 'no source/destination' unless refs.size > 1
            op.merge(refs)
          when :rm, :clean
            append_pathspec(op.extras, expect: flag == :rm)
          end
          source(sync: false, stderr: true)
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
            if cmd.respond_to?(:done)
              if from.nil? && (from = cmd.drop(1).find { |val| val.match?(/\A[a-z]{1,2}[a-z-]*\z/) })
                from = :"git:#{from}"
              end
              banner &&= cmd.temp { |val| val.start_with?(/--(?:work-tree|git-dir)/) }
            end
            from = nil if from == false
          end
          cmd = session_done cmd
          log&.info cmd
          banner = if banner
                     banner = (banner.is_a?(String) ? banner : cmd).gsub(File.join(path, ''), '')
                     format_banner(hint ? "#{banner} (#{hint})" : banner)
                   end
          on :first, from
          begin
            if io
              return `#{cmd}` if stdout

              return args ? [IO.popen(cmd), banner || '', from] : IO.popen(cmd)
            elsif stdin? ? sync : stdout
              print_item banner unless multiple
              ret = `#{cmd}`.chomp
              if !ret.empty?
                puts ret
              elsif success?(!banner.nil?)
                print_success
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
                    print_success if success?(n == 0 && !banner.nil?)
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
          grep = grep.empty? ? nil : matchmap(grep, prefix)
          sub = nil if stdin?
          ret = 0
          out = []
          data.each do |line|
            next if grep&.none? { |pat| pat.match?(line) }

            if block_given?
              line = yield line
              next unless line
            end
            if loglevel
              log&.add loglevel, line
            else
              sub&.each { |h| line = sub_style(line, **h) }
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

        def list_result(size, type, grep: [], action: 'found', from: nil)
          if size == 0
            puts empty_status("No #{type} were #{action}", 'grep', grep.join(', '))
          elsif stdout?
            styles = theme.fetch(:banner, []).reject { |s| s.to_s.end_with?('!') }
            styles << :bold if styles.size <= 1
            puts print_footer("#{size} #{size == 1 ? type.sub(/(?:(?<!l)e)?s\z/, '') : type}",
                              sub: [pat: /^(\d+)(.+)$/, styles: styles])
          end
          on :last, from
        end

        def choice_refs(msg, *type, format: nil, sort: '-creatordate', count: true, short: true, **kwargs)
          type << 'heads' if type.empty?
          unless format
            format = +"%(refname#{short ? ':short' : ''})"
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
          cmd << quote_option('format', '(%h) %s')
          cmd << basic_option('max-count', env('GIT_COUNT', ARG[:CHOICE])) if count
          choice_index('Choose a commit', git_spawn(cmd, stdout: false), column: /\((\w+)\)/, force: force, **kwargs)
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
            next if !pass.empty? && pass.any? { |val| File.fnmatch?(val, file, File::FNM_DOTMATCH) }

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

        def append_pull(opts, list, target: @session, flag: nil, no: nil, remote: nil, from: nil)
          target << '--force' if option('force', 'f', target: target)
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
                projectmap(split_escape(val)).each do |path|
                  target << basic_option('recurse-submodules', path)
                end
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

        def dryrun?(*, target: @session, **)
          return false unless target

          target.include?('--dry-run') || !option('dry-run', target: target).nil?
        end

        def quiet?(*, target: @session, **)
          return false unless target

          target.include?('--quiet') || (target.include?('-q') && stripext(target.first) == 'git')
        end

        def gitpath(*args)
          path.join('.git', *args)
        end

        def repotrack(origin, branch, quote: true)
          unless origin && branch && (i = origin.index('/'))
            raise_error(ArgumentError, "missing #{origin ? 'branch' : 'remote'} name", hint: origin)
          end
          branch = "#{branch}:#{origin[(i + 1)..-1]}" unless origin.end_with?("/#{branch}")
          [origin[0..(i - 1)], branch].tap { |ret| ret.map! { |val| shell_quote(val) } if quote }
        end

        def commithash(val)
          val[/\A:(\h{5,40})\z/, 1]
        end

        def commithead(val)
          return val unless (s = matchhead(val))

          s.start_with?(/\d/) ? "@~#{s}" : "@#{s}"
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
            values.find { |proj| proj.name == id } || values.find { |proj| proj.project == id } ||
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
      class << self
        def read_manifest(path)
          require 'rexml/document'
          return unless (file = path + '.repo/manifest.xml').exist?

          doc = REXML::Document.new(file.read)
          doc.elements['manifest/include'].attributes['name']&.sub('.xml', '')
        end
      end

      attr_reader :manifest_url, :manifest

      def repo(url, manifest = 'latest', run: nil, script: nil, args: nil, dev: nil, prod: nil,
               ref: @ref, group: @group)
        @home = if (val = env('REPO_HOME'))
                  path = Pathname.new(val)
                  if main == path.basename.to_s
                    @root = path.parent
                    if path.exist?
                      @root = nil unless path.directory?
                    elsif !@root.exist?
                      @root.mkpath
                    elsif !repo_install?
                      @root = nil unless repo_confirm
                    end
                  end
                  raise_error("path invalid: #{val}", hint: 'REPO_HOME') unless @root
                  path.realdirpath
                elsif (val = env('REPO_ROOT'))
                  @root = Pathname.new(val).realdirpath
                  if !@root.exist?
                    @root.mkpath
                  elsif !repo_install?(parent: true)
                    raise_error("path does not exist: #{val}", hint: 'REPO_ROOT') unless repo_confirm
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
          if script
            if (val = env('REPO_BUILD'))
              data[:script] = case val
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
                                val
                              end
              data[:env] = true
            else
              data[:script] = script
            end
            data[:args] = (val = env('REPO_SCRIPT')) ? shell_split(val, join: true) : args
          elsif (val = env('REPO_BUILD'))
            data[:run] = val
            data[:env] = true
          else
            data[:run] = run
          end
          data[:global] = true
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
          else
            data[:run] = run
          end
          data[:dev] = dev
          data[:prod] = prod
          script_set(data, group: group, ref: ref)
        end
        self
      end

      private

      def __repo__(**kwargs)
        kwargs.delete(:parallel) if env('REPO_SYNC', ignore: '0')

        namespace task_name('repo') do |ns|
          path = ns.scope.path
          branch = env('REPO_MANIFEST') || Repo.read_manifest(root)
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

            @project.select do |_, proj|
              next unless proj.enabled?(proj.workspace.baseref) && proj.global

              proj.depend(sync: true) if proj.depend?
              next if n == '2'

              proj.build?
            end
            .each_value do |proj|
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
            g = args.first && !opts.include?(args.first) ? args.shift : nil
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

      def repo_confirm
        return false unless root.directory?

        path = sub_style(root, styles: theme[:inline])
        @repo_override = Common::Prompt.confirm(
          "#{log_title(:warn)} \"#{path}\" is not empty. Continue with installation?", 'N',
          timeout: env('REPO_TIMEOUT').to_i.yield_self { |n| n > 0 ? n : 15 }
        )
      end

      def repo_run(cmd, exception: false)
        puts log_message(Logger::INFO, cmd, subject: main, hint: root) if verbose
        Common::System.shell(cmd, chdir: root, exception: exception)
      end

      def repo_bin
        Common::Shell.shell_bin('repo')
      end

      def repo_opts(*args)
        return args unless (n = ARGV.index('--'))

        ARGV[(n + 1)..-1].concat(args)
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
          common: %w[dry-run=!? include-workspace-root=!? loglevel=b workspaces=!? w|workspace=v].freeze,
          install: %w[package-lock-only=!? prefer-dedupe=!? E|save-exact=!? before=q cpu=b libc=b os=b].freeze,
          install_base: %w[audit=! bin-links=! foreground-scripts=!? fund=! ignore-scripts=!? install-links=!?
                           package-lock=! strict-peer-deps=!? include=b install-strategy=b omit=b].freeze,
          install_no: %w[audit bin-links fund package-lock].freeze,
          install_as: %w[no-save B|save-bundle D|save-dev O|save-optional save-peer P|save-prod g|global=!?
                         S|save=!?].freeze,
          run: %w[foreground-scripts=!? if-present=!? ignore-scripts=!? script-shell=p].freeze,
          exec: %w[c|call=q package=b].freeze,
          pack: %w[ignore-scripts=!? json=!? pack-destination=p].freeze
        }.freeze
        OPT_PNPM = {
          common: %w[aggregate-output color ignore-workspace-root-check no-color stream use-stderr C|dir=p loglevel=b
                     r|recursive w|workspace-root].freeze,
          cpu: %w[cpu=b libc=b os=b].freeze,
          filter: %w[fail-if-no-match changed-files-ignore-pattern=q filter=q filter-prod=q test-pattern=q].freeze,
          install: %w[fix-lockfile force ignore-pnpmfile ignore-workspace lockfile-only merge-git-branch-lockfiles
                      optimistic-repeat-install no-hoist no-lockfile no-optional prefer-frozen-lockfile resolution-only
                      shamefully-hoist side-effects-cache side-effects-cache-readonly s|silent strict-peer-dependencies
                      use-running-store-server use-store-server child-concurrency=i hoist-pattern=q lockfile-dir=p
                      modules-dir=p network-concurrency=i package-import-method=b public-hoist-pattern=q
                      reporter=b].freeze,
          install_base: %w[dangerously-allow-all-builds global-dir ignore-scripts offline prefer-offline store-dir=p
                           virtual-store-dir=p].freeze,
          install_no: %w[frozen-lockfile verify-store-integrity].freeze,
          install_as: %w[D|dev no-optional P|prod].freeze,
          update: %w[g|global i|interactive L|latest depth=i].freeze,
          dedupe: %w[check].freeze,
          run: %w[if-present no-bail parallel report-summary reporter-hide-prefix resume-from
                  sequential].freeze,
          exec: %w[no-reporter-hide-prefix parallel report-summary resume-from c|shell-mode].freeze,
          pack: %w[json pack-destination=p pack-gzip-level=i].freeze
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
          update: %w[A|audit C|caret E|exact L|latest T|tilde P|pattern=q S|scope=b].freeze,
          run: %w[scripts-prepend-node-path=b?].freeze
        }.freeze
        OPT_BERRY = {
          install: %w[check-cache check-resolutions immutable immutable-cache inline-builds json refresh-lockfile
                      mode=b].freeze,
          update: %w[C|caret E|exact F|fixed interactive T|tilde R|recursive mode=b].freeze,
          dedupe: %w[check json mode=b strategy=b].freeze,
          run: %w[B|binaries-only inspect inspect-brk T|top-level require=q].freeze,
          pack: %w[n|dry-run install-if-needed json o|out=p].freeze
        }.freeze
        private_constant :OPT_NPM, :OPT_PNPM, :OPT_YARN, :OPT_BERRY

        class << self
          def populate(*); end

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
          'package' => %i[install dedupe update].freeze,
          'outdated' => %i[major minor patch].freeze,
          'bump' => %i[version major minor patch].freeze,
          'publish' => %i[latest tag].freeze,
          'add' => nil,
          'run' => nil,
          'exec' => nil,
          'nvm' => nil,
          'pack' => nil
        })

        def initialize(*, init: nil, asdf: 'nodejs', **kwargs)
          super
          if @pass.include?(Node.ref)
            initialize_ref Node.ref
            initialize_logger(**kwargs)
          else
            initialize_build(Node.ref, prod: prod?, **kwargs)
            initialize_env(**kwargs)
          end
          @dependfile = basepath 'package.json'
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
                  format_desc action, nil, 'save?=prod|dev|optional|peer,name+'
                  task action, [:save] do |_, args|
                    packages = if args.save =~ /\A(=)?(prod|dev|optional|peer)\z/
                                 exact = !$1.nil?
                                 save = $2
                                 args.extras
                               else
                                 save = 'prod'
                                 args.to_a
                               end
                    param_guard(action, 'name', args: packages)
                    depend(:add, packages: packages, save: save, exact: exact)
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
                          if (item = list[n - 1])
                            run compose([item.first, extra].compact.join(' '), script: true)
                          elsif exception
                            indexerror n, list
                          else
                            log.warn "run script #{n} of #{list.size} (out of range)"
                            next
                          end
                        else
                          opts << val
                        end
                      end
                      next if opts.empty?

                      list = if (yarn = dependtype(:yarn)) > 0
                               yarn == 1 ? OPT_YARN[:run] + OPT_YARN[:common] : OPT_BERRY[:run]
                             elsif pnpm?
                               OPT_PNPM[:run] + OPT_PNPM[:filter] + OPT_PNPM[:common]
                             else
                               OPT_NPM[:run] + OPT_NPM[:common]
                             end
                      op = OptionPartition.new(opts, list, session(dependbin, 'run'), project: self)
                      op << op.extras.shift
                      op.append(delim: true, quote: false)
                      run(from: :run)
                    end
                  end
                when 'exec'
                  format_desc action, nil, 'pkg/cmd,opts*,args*'
                  task action, [:package] do |_, args|
                    if (package = args.package)
                      args = args.extras
                      if pnpm?
                        pre = ->(ch) { "-#{ch}" if (ch = args.delete(ch)) }
                        cmd = session 'pnpm', pre.call('r'), pre.call('c'), 'exec'
                        list = OPT_PNPM[:exec] + OPT_PNPM[:filter] + OPT_PNPM[:common]
                      else
                        cmd = session 'npm', 'exec'
                        list = OPT_NPM[:exec] + OPT_NPM[:common]
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
                      format_desc(action, flag, %w[update interactive dry-run], arg: 'opts?')
                      task flag do |_, args|
                        outdated flag, args.to_a
                      end
                    when 'package'
                      format_desc(action, flag, 'opts*', after: flag == :dedupe ? nil : 'name*')
                      task flag do |_, args|
                        package flag, args.to_a
                      end
                    when 'bump'
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
                      format_desc(action, flag, 'otp?,dry-run?,public|restricted?', before: flag == :tag ? 'tag' : nil)
                      task flag do |_, args|
                        args = args.to_a
                        dryrun = true if args.delete('dry-run') || args.delete('d')
                        if args.delete('public') || args.delete('p')
                          access = 'public'
                        elsif args.delete('restricted') || args.delete('r')
                          access = 'restricted'
                        end
                        if flag == :latest
                          otp = args.first
                        else
                          tag, otp = param_guard(action, flag, args: args)
                        end
                        publish(flag, otp: otp, tag: tag, dryrun: dryrun, access: access)
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
          print_item unless @output[0] || !verbose || task_invoked?(/^copy(?::#{Node.ref}|$)/)
          packed = false
          items.each do |dir|
            case dir
            when Pathname
              dest = dir
              @workspace.rev_clear(dest, sync: sync)
            when String
              dest = @workspace.root + dir
              @workspace.rev_clear(dest, sync: sync)
            when Symbol
              if (proj = @workspace.find(name: dir))
                @workspace.rev_clear(proj.name, sync: sync)
                dest = proj.path
              else
                log.warn message("copy project :#{dir}", hint: 'not found')
                dest = nil
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
              raise_error "copy: given #{dir}"
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
                         .map { |item| Pathname.new(item.first) }
                         .select(&:exist?)
                  end
                  .concat(Array(files))
                  packed = true
                end
                to = dest.join(into, npmname)
                to.mkpath
                log.info "cp npm:#{npmname} #{to}"
                subdir = []
                errors = 0
                files.each do |file|
                  s, d = file.is_a?(Array) ? file : [file, file]
                  dest = to + d
                  unless subdir.include?((target = dest.dirname).to_s)
                    target.mkpath
                    subdir << target.to_s
                  end
                  FileUtils.cp(basepath(s), dest, verbose: verbosetype > 0)
                rescue StandardError => e
                  print_error e
                  errors += 1
                end
              rescue StandardError => e
                on_error e, :copy
              else
                puts message(to, subdir.size, files.size - errors) if verbose
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
                      elsif (file = entry + 'package.json').exist?
                        begin
                          doc = JSON.parse(file.read)
                        rescue StandardError => e
                          log.error e
                          raise if exception
                        else
                          doc['name']
                        end
                      end
                if sub
                  target << [entry, dest.join(into, sub)]
                else
                  log.debug message("package.json in \"#{entry}\"", hint: 'not found')
                end
              end
            else
              target << [from, dest.join(into, scope || npmname)]
            end
            target.each do |src, to|
              glob.each { |val| log.info "cp #{from + val} #{to}" }
              copy_dir(src, to, glob, create: create, link: link, force: force, pass: pass, verbose: verbosetype > 0)
            rescue StandardError => e
              on_error e, :copy
            end
          end
          on :last, :copy
        end

        def depend(flag = nil, *, sync: invoked_sync?('depend', flag), packages: [], save: nil, exact: nil, **)
          if @depend && !flag
            super
          elsif outdated?
            workspace.rev_clear(name, sync: sync)
            return update if !flag && env('NODE_UPDATE')

            if (yarn = dependtype(:yarn)) > 0
              cmd = session 'yarn'
              if flag == :add
                cmd << 'add'
                cmd << "--#{save}" unless save == 'prod'
                cmd << '--exact' if exact
              else
                cmd << 'install'
                cmd << '--ignore-engines' if yarn == 1 && !option('ignore-engines', equals: '0')
              end
            elsif pnpm?
              cmd = session 'pnpm'
              if flag == :add
                cmd << 'add' << "--save-#{save}"
                cmd << '--save-exact' if exact
                option('allow-build') { |val| cmd << quote_option('allow-build', val) }
              else
                cmd << 'install'
                append_platform
              end
              option('public-hoist-pattern') do |val|
                split_escape(val).each { |opt| cmd << shell_option('public-hoist-pattern', opt) }
              end
              cmd << '--ignore-workspace' if env('NODE_WORKSPACES', equals: '0')
              cmd << '--dangerously-allow-all-builds' if option('approve-builds')
              append_nocolor
            else
              cmd = session 'npm', 'install'
              if flag == :add
                cmd << "--save-#{save}"
                cmd << '--save-exact' if exact
              else
                append_platform
              end
              cmd << '--workspaces=false' if env('NODE_WORKSPACES', equals: '0')
              cmd << '--package-lock=false' if option('package-lock', equals: '0')
              append_nocolor
            end
            append_loglevel
            cmd.merge(packages.map { |pkg| shell_quote(pkg) }) if flag == :add
            run(from: flag || :depend, sync: sync)
          end
        end

        def outdated(flag = nil, opts = [], sync: invoked_sync?('outdated', flag))
          dryrun = opts.include?('dry-run') || opts.include?('d')
          if pnpm?
            cmd = session 'pnpm', 'outdated'
            dryrun ||= dryrun?('pnpm')
          else
            cmd = session 'npm', 'outdated'
            dryrun ||= dryrun?('npm')
          end
          unless dryrun
            log.info cmd.to_s
            on :first, :outdated
          end
          banner = format_banner(cmd.temp(dryrun ? '--dry-run' : nil))
          print_item banner if sync
          begin
            data = pwd_set(dryrun: dryrun) { `#{cmd.temp('--json', '--loglevel=error')}` }
            doc = dependfile.read
            json = JSON.parse(doc)
          rescue StandardError => e
            on_error(e, :outdated, dryrun: dryrun)
            return
          else
            dep1 = json['dependencies'] || {}
            dep2 = json['devDependencies'] || {}
            target = json['name']
          end
          found = []
          avail = []
          rev = flag || (prod? ? :patch : :minor)
          inter = opts.include?('interactive') || opts.include?('i')
          unless data.empty?
            JSON.parse(data).each_pair do |key, val|
              val = val.find { |obj| obj['dependent'] == target } if val.is_a?(Array)
              next unless val && (file = dep1[key] || dep2[key]) && file != '*'

              latest = val['latest']
              ch = file[0]
              if ch.match?(/[~^]/)
                file = file[1..-1]
              elsif inter && rev == :major
                major = true
              else
                avail << [key, file, latest, true]
                next
              end
              current = val['current'] || file
              want = val['wanted']
              unless latest[SEM_VER, 6]
                case rev
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
              case rev
              when :major
                upgrade = a == '0' ? c == '0' || c == '1' : true
              when :minor
                upgrade = ch == '^' && (a == '0' ? c == '0' && b == d : a == c)
              when :patch
                upgrade = a == c && b == d && f[4] != w[4]
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
          size_col = ->(items, i) { items.map { |a| a[i] }.max_by(&:size).size }
          pad_ord = lambda do |val, ord|
            ret = val.succ.to_s
            ord.size > 9 ? ret.rjust(ord.size.to_s.size) : ret
          end
          footer = lambda do |val, size|
            next unless verbose

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
            col1 = size_col.call(found, 0) + 4
            col2 = size_col.call(found, 1) + 4
            found.each_with_index do |item, i|
              a, b, c, d, e = item
              f = inter && (rev != :major || e || semmajor?(item[5], item[6]))
              if f && !confirm_outdated(a, c, (d / 2.0).ceil, b, lock: e, col1: col1)
                cur = -1
              else
                cur = modified
                doc.sub!(/("#{Regexp.escape(a)}"\s*:\s*)"([~^])#{e ? '?' : ''}#{Regexp.escape(b)}"/) do |capture|
                  if $2 == '~' && rev != :patch
                    cur = -1
                    pending += 1
                    capture
                  else
                    modified += 1
                    "#{$1}\"#{$2 || (d == 1 && e ? '^' : '')}#{c}\""
                  end
                end
              end
              a = a.ljust(col1)
              b = b.ljust(col2)
              b = sub_style(b, styles: theme[:current]) if theme[:current]
              c = if cur == -1
                    'SKIP'
                  elsif modified == cur
                    'FAIL'
                  elsif d == 1
                    a = sub_style(a, styles: theme[:major])
                    sub_style(c, :bold, styles: color(:green))
                  else
                    sub_style(c, pat: SEM_VER, styles: color(d == 3 ? :green : :yellow), index: d)
                  end
              puts "#{pad_ord.call(i, found)}. #{a + b + c}"
            end
            pending = avail.reduce(pending) { |a, b| a + (b[3] ? 0 : 1) }
            if dryrun || (modified == 0 && pending > 0)
              footer.call(modified, found.size)
            elsif modified > 0
              modified = -1
              File.write(dependfile, doc)
              if opts.include?('update') || opts.include?('u') || option('update')
                update
              else
                footer.call(0, found.size)
              end
              printsucc
              commit(:add, ['package.json'], pass: true)
            end
          elsif !avail.empty?
            col1 = size_col.call(avail, 0) + 4
            col2 = size_col.call(avail, 1)
            col3 = size_col.call(avail, 2) + 4
            avail.each_with_index do |item, i|
              a, b, c, d = item
              a = a.ljust(col1)
              b = sub_style(b.ljust(col2), styles: color(d ? :red : :yellow))
              c = c.ljust(col3)
              unless d
                a = sub_style(a, styles: theme[:active])
                c = sub_style(c, styles: color(:green))
                pending += 1
              end
              puts "#{pad_ord.call(i, avail)}. #{a + c + b} (#{d ? 'locked' : 'latest'})"
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
          unless version && !read_packagemanager(:private)
            print_error('invalid task "publish"', subject: name, hint: version ? 'private' : nil)
            return
          end
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
            from = :publish
            log.info cmd.to_s
          end
          if sync
            run(sync: sync, from: from, interactive: !dryrun && "Publish #{sub_style(npmname, styles: theme[:active])}")
          else
            require 'open3'
            on :first, from
            pwd_set(from: from, dryrun: dryrun) do
              cmd = session_done cmd
              Open3.popen2e(cmd) do |_, out|
                write_lines(out, banner: format_banner(cmd),
                                 sub: npmnotice + [pat: /^(.+)(Tarball .+)$/, styles: color(:bright_blue), index: 2])
              end
            end
            on :last, from
          end
        end

        def package(flag, opts = [], from: nil)
          workspace.rev_clear(name)
          if (yarn = dependtype(:yarn)) > 0
            cmd = session 'yarn', if flag == :update
                                    yarn == 1 ? 'upgrade' : 'up'
                                  else
                                    yarn == 1 && flag == :dedupe ? 'install' : flag
                                  end
            op = OptionPartition.new(opts, if yarn == 1
                                             OPT_YARN.fetch(flag == :dedupe ? :install : flag, []) + OPT_YARN[:common]
                                           else
                                             OPT_BERRY[flag]
                                           end, cmd, project: self)
            op << '--ignore-engines' if yarn == 1 && !option('ignore-engines', equals: '0')
            op.clear
            append_loglevel
          else
            if pnpm?
              cmd = session 'pnpm', flag
              list = OPT_PNPM[:install_base] + OPT_PNPM.fetch(flag, []) + OPT_PNPM[:common]
              list.concat(OPT_PNPM[:install_as] + OPT_PNPM[:filter]) unless flag == :dedupe
              list.concat(OPT_PNPM[:cpu]) unless flag == :update
              no = OPT_PNPM[:"#{flag}_no"]
            else
              cmd = session 'npm', flag
              list = OPT_NPM[:install_base] + OPT_NPM.fetch(flag, []) + OPT_NPM[:common]
              list.concat(OPT_NPM[:install_as]) if flag == :install || flag == :update
              no = OPT_NPM[:install_no]
            end
            op = OptionPartition.new(opts, list, cmd, no: no, project: self)
            op.each do |opt|
              if opt =~ op.values
                case $1
                when 'w', 'workspace'
                  op << ($2.match?(%r{[\\/]}) ? quote_option($1, basepath($2)) : shell_option($1, $2))
                end
              elsif opt.include?('=')
                op.errors << opt
              else
                op.found << opt
              end
            end
            op.swap
            append_platform if flag == :install
            append_nocolor
            append_loglevel
            if flag == :dedupe || pnpm?
              op.clear
            else
              op.append(quote: true)
            end
          end
          op.clear(errors: true)
          run(from: from || :"package:#{flag}")
        end

        def bump(flag, val = nil)
          return unless (cur = version)

          if flag == :version
            return unless val
          else
            seg = semscan(cur, fill: false)
            case flag
            when :major
              if seg[0] != '0' || seg[2].nil?
                seg[0] = seg[0].succ
                seg[2] = '0'
              else
                seg[2] = seg[2].succ
              end
              seg[4] = '0'
            when :minor
              if seg[0] == '0'
                seg[4] &&= seg[4].succ
              else
                seg[2] = seg[2].succ
                seg[4] &&= '0'
              end
            when :patch
              seg[4] &&= seg[4].succ
            end
            return if (val = seg.join) == cur
          end
          begin
            doc = dependfile.read
            if doc.sub!(/"version"\s*:\s*"#{cur}"/, "\"version\": \"#{val}\"")
              unless dryrun?
                dependfile.write(doc)
                log.info "bump version #{cur} to #{val} (#{flag})"
                on :first, :bump
              end
              if stdin?
                puts val
              elsif verbose
                major = flag == :major
                emphasize("version: #{val}", title: name, border: borderstyle, sub: [
                  headerstyle,
                  { pat: /\A(version:)( )(\S+)(.*)\z/, styles: color(major ? :green : :yellow), index: 3 },
                  { pat: /\A(version:)(.*)\z/, styles: theme[major ? :major : :active] }
                ])
              end
              on :last, :bump unless dryrun?
            else
              raise_error('version not found', hint: dependfile)
            end
          rescue StandardError => e
            on_error(e, :bump, dryrun: dryrun?)
          end
        end

        def pack(opts = [])
          return unless version

          cmd = session dependbin, 'pack'
          if dependtype(:yarn) > 1
            op = OptionPartition.new(opts, OPT_BERRY[:pack], cmd, project: self)
            op.append?('out', Pathname.pwd + "#{project}-#{version}.tgz")
          else
            op = OptionPartition.new(opts, pnpm? ? OPT_PNPM[:pack] : OPT_NPM[:pack] + OPT_NPM[:common], cmd,
                                     project: self)
            unless pnpm?
              op.each do |opt|
                next unless opt =~ op.values

                case $1
                when 'w', 'workspace'
                  op << ($2.match?(%r{[\\/]}) ? quote_option($1, basepath($2)) : shell_option($1, $2))
                  op.found << opt
                end
              end
            end
            op.append?('pack-destination', Dir.pwd)
          end
          op.clear
          run(from: :pack)
        end

        def compose(target, opts = nil, script: false, args: nil, from: nil, **)
          return unless target

          if script
            ret = session dependbin, 'run'
            raise_error("#{dependbin} run: given #{target}", hint: from) unless append_any(target, build: true)
            append_any opts
            append_loglevel
            append_any(args, delim: true)
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
              raise_error("compose: given #{target}", hint: from)
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
          (@pm[:yarn] ||= if rootpath('yarn.lock', ascend: dependext).exist?
                            yarntype
                          elsif (ver = read_packagemanager || read_install)
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
          (@pm[:pnpm] ||= if rootpath('pnpm-lock.yaml', ascend: dependext).exist?
                            pnpmtype
                          elsif (ver = read_packagemanager || read_install)
                            ver.start_with?('pnpm') ? pnpmtype : 0
                          else
                            @pm[:__] == 'pnpm' ? pnpmtype : 0
                          end) > 0
        end

        def workspaces?
          if pnpm?
            basepath('pnpm-workspace.yaml').exist?
          else
            read_packagemanager(:workspaces).is_a?(Array)
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
          @version ||= read_packagemanager(:version)
        end

        def packagename
          read_packagemanager :name
        end

        def scripts
          @scripts ||= read_packagemanager(:scripts).yield_self { |ret| ret.is_a?(Hash) ? ret : {} }
        end

        private

        def read_packagemanager(key = nil, version: nil, update: false)
          if (key ? !@pm.key?(key) : @pm[:_].nil?) || update
            doc = JSON.parse(dependfile.read)
            if @pm[:_].nil?
              @pm[:_] = (val = doc['packageManager']) ? val[0, val.index('+') || val.size] : false
              @pm[:name] = doc['name']
              @pm[:scripts] = doc['scripts']
              @pm[:version] = doc['version']
              @pm[:private] = doc['private']
              @pm[:workspaces] = doc['workspaces']
            end
            @pm[key] = doc[key.to_s] if key
          end
        rescue StandardError => e
          log.debug e
          @pm[:_] = false
          nil
        else
          if key
            @pm[key]
          elsif (ret = @pm[:_]) && (!version || semgte?(ret[(ret.index('@') + 1)..-1], version))
            ret
          end
        end

        def read_install
          return unless (ret = env('NODE_INSTALL'))

          if ret.include?(',')
            catch :found do
              split_escape(ret).each do |val|
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
          @pm[:_] ||= ret
          ret
        end

        def yarntype(exist: false)
          if (rc = rootpath('.yarnrc.yml', ascend: dependext)).exist?
            require 'yaml'
            doc = YAML.load_file(rc)
            doc.nodeLinker == 'node-modules' ? 2 : 3
          elsif exist && !basepath('yarn.lock').exist?
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
          doc = YAML.load_file(basepath('node_modules/.modules.yaml', ascend: dependext))
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
            %w[pnpm-lock.yaml pnpm-workspace.yaml].any? { |val| basepath(val).exist? } ? 4 : 0
          else
            log.debug e
            4
          end
        end

        def append_loglevel(target: @session)
          level = env('NODE_LOGLEVEL')
          silent = verbosetype == 0 || level == 'silent'
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
          %w[cpu os libc].each { |name| option(name) { |val| target << basic_option(name, val) } }
        end

        def dependext
          'package.json' if parent&.has?('outdated', Node.ref)
        end

        def npmname
          packagename || project
        end

        def npmnotice
          [
            { pat: /^(npm error )(code|\d+)(.+)$/, styles: color(:bright_cyan), index: 2 },
            { pat: /^(npm )(error)(.*)$/, styles: color(:bright_red), index: 2 },
            { pat: /^(npm )(notice)(.*)$/, styles: color(:bright_cyan), index: 2 },
            { pat: /^(npm )(.+)$/, styles: :bold }
          ]
        end

        def dryrun?(prefix = dependbin, **)
          super || !option('dry-run', prefix: prefix).nil?
        end
      end

      Application.implement Node

      class Python < Git
        DEP_PYTHON = %w[poetry.lock setup.cfg pyproject.toml setup.py requirements.txt].freeze
        DIR_PYTHON = (DEP_PYTHON + %w[README.rst]).freeze
        OPT_PYTHON = {
          common: %w[b B d E h i I O P q s S u v x c=q m=b W=b X=q check-hash-based-pycs=b].freeze,
          build: %w[C=bm n|no-isolation s|sdist x|skip-dependency-check v|verbose w|wheel config-setting=q installer=b
                    o|outdir=p].freeze,
          venv: %w[clear copies symlinks system-site-packages upgrade upgrade-deps without-scm-ignore-files without-pip
                   prompt=q].freeze
        }.freeze
        OPT_PIP = {
          common: %w[debug disable-pip-version-check isolated no-cache-dir no-color no-input no-python-version-warning
                     q|quiet require-virtualenv v|verbose cache-dir=p cert=p client-cert=p exists-action=b log=p proxy=q
                     python=q retries=i timeout=i trusted-host=b use-deprecated=b use-feature=b].freeze,
          install: %w[break-system-packages check-build-dependencies compile dry-run force-reinstall I|ignore-installed
                      ignore-requires-python no-build-isolation no-clean no-compile no-deps no-index no-warn-conflicts
                      no-warn-script-location pre prefer-binary require-hashes U|upgrade use-pep517 user abi=b
                      config-settings=q c|constraint=p e|editable=v extra-index-url=q f|find-links=q global-option=q
                      implementation=b i|index-url=q no-binary=q only-binary=q platform=q prefix=p progress-bar=b
                      python-version=q report=p r|requirement=p root=p root-user-action=b src=p t|target=p
                      upgrade-strategy=b].freeze,
          uninstall: %w[break-system-packages y|yes r|requirement=p root-user-action=b].freeze,
          freeze: %w[all exclude-editable l|local user exclude=b path=p r|requirement=p].freeze
        }.freeze
        OPT_POETRY = {
          common: %w[ansi no-ansi no-cache n|no-interaction no-plugins q|quiet v|verbose P|project=p].freeze,
          build: %w[clean config-settings=qq f|format=b o|output=p].freeze,
          publish: %w[build dry-run skip-existing cert=p client-cert=p dist-dir=p p|password=q r|repository=q
                      u|username=qq].freeze
        }.freeze
        OPT_PDM = {
          common: %w[I|ignore-python no-cache n|non-interactive].freeze,
          build: %w[C=bm no-clean no-isolation no-sdist no-wheel quiet verbose config-setting=q d|dest=p p|project=p
                    k|skip=b].freeze,
          publish: %w[no-build no-very-ssl quiet S|sign skip-existing verbose ca-certs=p c|comment=q d|dest=p
                      i|identity=b P|password=q p|project=p r|repository=q k|skip=b u|username=qq].freeze
        }.freeze
        OPT_HATCH = {
          common: %w[color interactive no-color no-interactive cache-dir=p config=p data-dir=p e|env=b p|project=b
                     q|quiet v|verbose].freeze,
          build: %w[clean-hooks-after ext hooks-only no-hooks c|clean t|target=b].freeze,
          publish: %w[initialize-auth n|no-prompt y|yes a|auth=q ca-cert=p client-cert=p client-key=p o|option=q
                      p|publisher=b r|repo=q u|user=q].freeze
        }.freeze
        OPT_TWINE = {
          publish: %w[attestations disable-progress-bar non-interactive s|sign skip-existing verbose cert=p
                      client-cert=p c|comment=q config-file=p i|identity=b p|password=q r|repository=b repository-url=q
                      sign-with=b u|username=qq].freeze
        }.freeze
        private_constant :DEP_PYTHON, :DIR_PYTHON, :OPT_PYTHON, :OPT_PIP, :OPT_POETRY, :OPT_PDM, :OPT_HATCH, :OPT_TWINE

        class << self
          def populate(*); end

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
          'pip' => %i[upgrade uninstall freeze].freeze,
          'install' => %i[user force upgrade target editable].freeze,
          'outdated' => %i[major minor patch].freeze,
          'build' => %i[python poetry pdm hatch].freeze,
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
                    %w[tool.poetry.scripts tool.pdm.scripts project.scripts].each_with_index do |table, index|
                      next if (list = read_pyproject(table)).empty?

                      if args.command == '#'
                        format_list(list, "run[#{indexchar}N]", 'scripts', grep: args.extras, from: pyprojectfile)
                        found |= 1
                      else
                        args.to_a.each do |val|
                          if (n, = indexitem(val))
                            if (script, = list[n - 1])
                              case index
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
                              log.warn "run script #{n} of #{list.size} (out of range)"
                            end
                          else
                            case index
                            when 0
                              found |= 1
                              run(session_output('poetry', 'run', val), from: :run)
                            when 1
                              found |= 1
                              run(pdm_session('run', val), from: :run)
                            else
                              raise_error "script: #{val}" if exception
                              found |= 2
                              log.warn "run script \"#{val}\" (not indexed)"
                            end
                          end
                        end
                      end
                      break
                    end
                    unless found.anybits?(1)
                      puts log_message(found == 0 ? Logger::INFO : Logger.WARN,
                                       "no scripts #{found == 0 ? 'found' : 'executed'}",
                                       subject: name, hint: pyprojectfile)
                    end
                  end
                when 'exec'
                  format_desc action, nil, 'command|:,args*'
                  task action do |_, args|
                    i = (args = args.to_a).delete(':')
                    cmd = if i && !workspace.windows?
                            readline('Enter script', force: true, multiline: ['##', ';'])
                          elsif i || args.empty?
                            readline('Enter command', force: true)
                          else
                            if (val = command_args(args, min: 1, prefix: 'python'))
                              args << val
                            end
                            args.join(' ')
                          end
                    shell(cmd, name: :exec, chdir: path)
                  end
                end
              else
                namespace action do
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

                          format_desc action, flag, 'c|create?,d|depend?'
                          task flag do |_, args|
                            rm_rf(venv, verbose: true)
                            venv_init if has_value?(%w[c create], args.to_a)
                            depend if has_value?(%w[d depend], args.to_a)
                          end
                        when :exec
                          format_desc action, flag, 'command,args*'
                          task flag do |_, args|
                            args = args.to_a
                            if args.empty?
                              args = readline('Enter command', force: true).split(' ', 2)
                            elsif args.size == 1 && !option('interactive', prefix: 'venv', equals: '0')
                              args << readline('Enter arguments', force: false)
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
                          if (file = pip(flag, args.to_a)) && verbose
                            puts File.read(file)
                          end
                        end
                      when :uninstall
                        format_desc action, flag, 'package+,opts*'
                        task flag do |_, args|
                          pip flag, args.to_a
                        end
                      end
                    when 'install'
                      format_desc(action, flag, 'opts*', before: case flag
                                                                 when :target then 'dir'
                                                                 when :editable then 'path/url?'
                                                                 when :upgrade then 'strategy?,package+'
                                                                 end)
                      case flag
                      when :editable
                        task flag do |_, args|
                          install flag, args.to_a
                        end
                      when :upgrade
                        task flag, [:strategy] do |_, args|
                          args = case (strategy = args.strategy)
                                 when 'eager', 'only-if-needed'
                                   args.extras
                                 when 'needed'
                                   strategy = 'only-if-needed'
                                   args.extras
                                 else
                                   strategy = nil
                                   args.to_a
                                 end
                          install(flag, args, strategy: strategy)
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
                      format_desc action, flag, 'eager?,user?'
                      task flag do |_, args|
                        outdated flag, args.to_a
                      end
                    when 'build'
                      case flag
                      when :poetry
                        next unless build_backend == 'poetry.core.masonry.api'
                      when :pdm
                        next unless build_backend == 'pdm.backend'
                      when :hatch
                        next unless build_backend == 'hatchling.build'
                      end
                      format_desc(action, flag, 'opts*', after: case flag
                                                                when :poetry then 'output?'
                                                                when :pdm then 'dest?'
                                                                when :hatch then 'location?'
                                                                else 'outdir?'
                                                                end)
                      task flag do |_, args|
                        build! flag, args.to_a
                      end
                      break unless flag == :python
                    when 'publish'
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
                append_pip(flag, opts, from: :install)
              else
                append_global
              end
              cmd << "-r #{DEP_PYTHON[4]}" if basepath(DEP_PYTHON[4]).exist? && !session_arg?('r', 'requirement')
              append_editable
            end
            run(from: :depend, sync: sync)
          end
        end

        def outdated(flag = nil, opts = [], sync: invoked_sync?('outdated', flag))
          cmd = pip_session 'list --outdated'
          append_global
          cmd = session_done cmd
          log.info cmd
          on :first, :outdated
          banner = format_banner cmd
          print_item banner if sync
          start = 0
          found = 0
          major = []
          minor = []
          patch = []
          pwd_set(from: :outdated) do
            buffer = []
            out = ->(val) { sync ? puts(val) : buffer << val }
            if workspace.windows?
              (venv ? command(runenv, cmd) : `#{cmd}`).lines
            else
              IO.popen(runenv || {}, cmd)
            end.each do |line|
              next if line.match?(/^[ -]+$/)

              if start > 0
                unless stdin?
                  cur, lat = line.scan(SEM_VER)
                  next unless cur && lat

                  latest = lat.join
                  current = cur.join
                  semver cur
                  semver lat
                  name = line.split(' ', 2).first
                  type = if semmajor?(cur, lat)
                           major << name
                           2
                         elsif cur[2] == lat[2]
                           patch << name
                           0
                         else
                           minor << name
                           1
                         end
                  if type == 0
                    styles = color(:yellow)
                  else
                    styles = color(:green)
                    line = sub_style(line, pat: /^(\S+)(.+)$/, styles: if type == 2
                                                                         styles << :bold
                                                                         theme[:major]
                                                                       else
                                                                         theme[:active]
                                                                       end)
                  end
                  if theme[:current]
                    line = sub_style(line, pat: /^(.+)(#{Regexp.escape(current)})(.+)$/, styles: theme[:current],
                                           index: 2)
                  end
                  line = sub_style(line, pat: /^(.+)(#{Regexp.escape(latest)})(.+)$/, styles: styles, index: 2)
                  found += 1
                end
                out.call("#{start.to_s.rjust(2)}. #{line}")
                start += 1
              elsif line.start_with?('Package')
                unless stdin?
                  sub = { pat: /^(.*)(?<!\dm)(Package|Latest)(.+)$/, styles: theme[:header], index: 2 }
                  out.call(print_footer(" #  #{line.chomp}", reverse: true, sub: [sub, sub]))
                end
                start += 1
              end
            end
            unless sync
              print_item banner
              puts buffer
            end
            if found > 0
              print_status(major.size, minor.size, patch.size, from: :outdated)
              pkg = case flag
                    when :major
                      major + minor + patch
                    when :minor
                      minor + patch
                    when :patch
                      patch
                    end
              unless !pkg || pkg.empty?
                install(:upgrade, pkg, strategy: opts.include?('eager') ? 'eager' : nil, user: opts.include?('user'))
              end
            elsif start == 0
              puts 'No updates were found'
            end
          end
          on :last, :outdated
        end

        def install(flag, opts = [], strategy: nil, user: nil)
          cmd = pip_session 'install'
          out = append_pip(flag, opts, from: :install)
          case flag
          when :editable
            cmd << quote_option('e', out.pop || editable || '.')
            option_clear out
          when :upgrade
            raise_error('no packages listed', hint: flag) if out.empty?
            cmd << '--upgrade'
            cmd << '--user' if user
            cmd << basic_option('upgrade-strategy', strategy) if strategy
            append_value out
            python_session('-m pip', *cmd.to_a.drop(1)) if workspace.windows?
          end
          run(from: :install)
        end

        def build!(flag, opts = [])
          case flag
          when :poetry
            cmd = poetry_session 'build'
            list = OPT_POETRY[:build] + OPT_POETRY[:common]
          when :pdm
            cmd, opts = pdm_session('build', opts: opts)
            list = OPT_PDM[:build]
          when :hatch
            cmd, opts = hatch_session('build', opts: opts)
            list = OPT_HATCH[:build]
          else
            cmd, opts = python_session('-m build', opts: opts)
            list = OPT_PYTHON[:build]
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
          case flag
          when :poetry
            poetry_session 'publish'
            list = OPT_POETRY[:publish] + OPT_POETRY[:common]
          when :pdm
            opts = pdm_session('publish', opts: opts).last
            list = OPT_PDM[:publish]
          when :hatch
            opts = hatch_session('publish', opts: opts).last
            list = OPT_HATCH[:publish]
          when :twine
            session 'twine', 'upload'
            list = OPT_TWINE[:publish]
          end
          op = OptionPartition.new(opts, list, @session, project: self, single: singleopt(flag))
          dist = lambda do
            basepath('dist').tap do |dir|
              raise_error('no source files found', hint: dir) unless dir.directory? && !dir.empty?
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
          when :hatch, :twine
            if op.empty?
              op.push("#{dist.call}/*")
            else
              op.map! { |val| basepath(val) }
            end
            op.append
          else
            dist.call unless op.arg?(*(flag == :poetry ? ['dist-dir'] : ['d', 'dest']))
            op.clear(pass: false)
          end
          run(from: :"#{flag}:publish", interactive: "Publish #{sub_style(project, styles: theme[:active])}")
        end

        def pip(flag, opts = [])
          cmd = pip_session flag
          out = append_pip(nil, opts, from: flag)
          case flag
          when :uninstall
            raise_error('no packages listed', hint: flag) if out.empty?
            cmd.merge(out)
          when :freeze
            venv_init
            ret = basepath(out.shift || DEP_PYTHON[4])
            cmd << '>' << shell_quote(ret)
            option_clear out
          end
          run(from: :"pip:#{flag}")
          ret
        end

        def variable_set(key, *val, **, &blk)
          if block_given?
            case key
            when :dependfile, :venv, :editable
              val = block_args val, &blk
            end
          end
          case key
          when :dependfile
            if val.first.nil?
              super
            else
              req = basepath(*val)
              if (index = DEP_PYTHON.index(req.basename.to_s))
                @dependindex = index
                @dependfile = req
              else
                log.warn "variable_set: @#{key}=#{req} (not supported)"
              end
            end
          when :editable
            editable_set val.first
          when :venv
            @venv = val.empty? || val.first.nil? ? nil : basepath(*val)
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

        def python_session(*cmd, opts: nil)
          pre = preopts(quiet: false)
          return session('python', *pre, *cmd, path: venv.nil?) unless opts

          op = OptionPartition.new(opts, OPT_PYTHON[:common], project: self, single: singleopt(:python))
          ret = session('python', *pre, *op.to_a, *cmd, path: venv.nil?)
          [ret, op.extras]
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
          ret = session(name, *preopts, *op.to_a, *cmd, path: venv.nil?)
          [ret, op.extras]
        end

        def append_pip(flag, opts, target: @session, from: nil)
          unless from && !opts.empty?
            append_global(target: target)
            return []
          end
          op = OptionPartition.new(opts, OPT_PIP[from] + OPT_PIP[:common], target, project: self, single: singleopt)
          append_global(target: target)
          if from == :install
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
            op << '--no-build-isolation' if option('build-isolation', equals: '0')
            op.swap
            if edit
              edit = basepath(edit) unless %r{\A[a-z]+(?:\+[a-z]+)?://}i.match?(edit)
              if flag == :editable
                op.push(edit)
              else
                op << quote_option('e', edit)
              end
            end
            case flag
            when :editable, :upgrade
              op.extras
            else
              op.clear
              []
            end
          else
            op.extras
          end
        end

        def append_editable(target: @session)
          return if requirements? && editable == '.'

          if (val = option('editable', 'e', target: target, ignore: false))
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
          option('cache-dir', target: target) do |val|
            target << case val
                      when '0', 'false'
                        '--no-cache-dir'
                      else
                        quote_option('cache-dir', basepath(val))
                      end
          end
          option('proxy', target: target) { |val| target << quote_option('proxy', val) }
          option('python', target: target) { |val| target << quote_option('python', basepath(val)) }
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
                    val = case (val = data[2])
                          when 'true'
                            true
                          when 'false'
                            false
                          else
                            val.include?('.') ? val.to_f : val.to_i
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
          return unless (ret = basepath(DEP_PYTHON[2])).exist?

          ret
        end

        def singleopt(flag = nil)
          case flag
          when :python
            /\A(?:v+|q+|b+|V+|O+)\z/
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

          if val.is_a?(Array)
            val, *opts = val
            @venvopts = opts
          end
          @venv = basepath(val)
          if projectpath?(@venv)
            if @venv.exist?
              log.debug "venv found: #{@venv}"
            elsif @path.directory? && !@path.empty?
              @venv.mkpath
              log.info "venv mkdir: #{@venv}"
            end
          elsif !@venv.directory?
            log.warn "venv invalid: #{@venv}"
            @venv = nil
          end
        end

        def venv_init
          return if !venv || (venvbin.directory? && !venvbin.empty?)

          puts log_message(Logger::INFO, venv, subject: 'venv', hint: 'init')
          opts = @venvopts&.map { |val| OptionPartition.strip(val) }&.flatten
          venv_create(venv, opts || ["prompt=#{name}", 'upgrade-deps'], env: false, banner: false)
          puts log_message(Logger::INFO, venv, subject: 'venv', hint: 'created')
        end

        def venv_create(dir, opts = [], env: nil, banner: true)
          cmd, opts = python_session('-m venv', opts: opts)
          op = OptionPartition.new(opts, OPT_PYTHON[:venv], cmd, project: self)
          status = op.append(dir, delim: true)
                     .clear(pass: false)
                     .arg?(/\A-v+\z/)
          run(op, env, exception: true, banner: banner)
          install(:upgrade, ['poetry']) if poetry?
          puts(dir.directory? ? "Success: #{dir}" : 'Failed') if banner && !status
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
        DIR_RUBY = (GEMFILE + Rake::Application::DEFAULT_RAKEFILES + ['README.rdoc']).freeze
        OPT_RUBY = {
          ruby: %w[0=im? a c C=pm e=q E=bm F=qm i=bm? I=pm l n p r=bm s S w W=bm? x=pm? d|debug jit rjit verbose
                   y|yydebug backtrace-limit=i crash-report=q disable=q dump=q enable=q encoding=b external-encoding=b
                   internal-encoding=b parser=b].freeze,
          rake: %w[A|all B|build-all comments n|dry-run m|multitask P|prereqs q|quiet X|no-deprecation-warnings
                   N|no-search G|no-system nosearch nosystem rules s|silent g|system v|verbose backtrace=b?
                   D|describe=q? e|execute=q E|execute-continue=q p|execute-print=q job-stats=b? j|jobs=i? I|libdir=p
                   R|rakelib=p rakelibdir=p r|require=b suppress-backtrace=q T|tasks=q? t|trace=b? W|where=q?].freeze,
          irb: %w[d f U w E=b I=p r=b W=im? autocomplete colorize echo echo-on-assignment extra-doc-dir inf-ruby-mode
                  inspect multiline no-pager noautocomplete nocolorize noecho noecho-on-assignment noinspect
                  nomultiline noprompt noscript nosingleline noverbose regexp-completor sample-book-mode script
                  simple-prompt single-irb singleline tracer truncate-echo-on-assignment type-completor verbose
                  back-trace-limit=i context-mode=i prompt=b prompt-mode=b].freeze
        }.freeze
        OPT_BUNDLE = {
          common: %w[no-color V|verbose r|retry=i].freeze,
          install: %w[frozen no-cache no-prune system binstubs=p? path=p standalone=q? target-rbconfig=p trust-policy=b
                      with=q without=q].freeze,
          install_base: %w[force full-index local quiet redownload gemfile=p j|jobs=i].freeze,
          update: %w[all conservative local major minor patch pre ruby strict bundler=b? g|group=q source=b].freeze,
          outdated: %w[filter-major filter-minor filter-patch filter-strict groups local parseable porcelain pre
                       only-explicit strict update-strict group=q source=b].freeze,
          exec: %w[gemfile=p].freeze,
          cache: %w[all all-platforms frozen no-all no-install no-prune quiet cache-path=p gemfile=p path=p].freeze,
          check: %w[dry-run gemfile=p path=p].freeze
        }.freeze
        OPT_GEM = {
          common: %w[backtrace debug q|quiet no-verbose norc silent V|verbose config-file=p].freeze,
          install: %w[version=q].freeze,
          install_base: %w[E f w b|both clear-sources conservative default development development-all explain
                           ignore-dependencies l|local N|no-document r|remote vendor n|bindir=p build-root=p
                           B|bulk-threshold=i document=b? g|file=p? p|http-proxy=q? i|install-dir=p platform=q
                           s|source=q target-rbconfig=p? P|trust-policy=b without=q].freeze,
          update: %w[system=b?].freeze,
          uninstall: %w[a D I x vendor n|bindir=p i|install-dir=p platform=b v|version=q].freeze,
          outdated: %w[b|both clear-sources l|local r|remote B|bulk-threshold=i p|http-proxy=q? platform=q
                       source=q].freeze,
          push: %w[attestation=p host=q p|http-proxy=q? k|key=b otp=b].freeze,
          build: %w[C=p force strict o|output=p platform=q].freeze,
          exec: %w[conservative g|gem=b v|version=q].freeze,
          pristine: %w[E all only-executables only-missing-extensions only-plugins n|bindir=p i|install-dir=p skip=b
                       v|version=q].freeze,
          no: {
            install: %w[env-shebang force format-executable http-proxy lock minimal-deps post-install-message prerelease
                        suggestions user-install wrappers].freeze,
            uninstall: %w[abort-on-dependent all check-development executables force format-executable
                          ignore-dependencies user-install].freeze,
            outdated: %w[http-proxy].freeze,
            push: %w[http-proxy].freeze,
            exec: %w[prerelease].freeze,
            pristine: %w[env-shebang extensions].freeze
          }.freeze
        }.freeze
        private_constant :GEMFILE, :DIR_RUBY, :OPT_RUBY, :OPT_BUNDLE, :OPT_GEM

        class << self
          def populate(*); end

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
          'install' => %i[redownload local prefer-local].freeze,
          'update' => %i[patch minor major all].freeze,
          'outdated' => %i[patch minor major].freeze,
          'gem' => %i[install uninstall update pristine outdated build push exec].freeze,
          'ruby' => %i[file script version].freeze,
          'exec' => nil,
          'cache' => nil,
          'config' => nil,
          'check' => nil,
          'rake' => nil,
          'irb' => nil
        })

        attr_reader :gemdir

        def initialize(*, autodetect: false, gemspec: nil, asdf: 'ruby', **kwargs)
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
          return if !@output[0].nil? || !@copy.nil? || version || @autodetect || !rakefile

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

                  format_desc action, nil, "task+,opts*|#{indexchar}index+|#,pattern*"
                  task action, [:command] do |_, args|
                    if args.command == '#'
                      format_list(raketasks, "rake[#{indexchar}N]", 'tasks', grep: args.extras, from: rakefile,
                                                                             each: ->(val) { val[0] + val[1].to_s })
                    else
                      args, opts = args.to_a.partition { |val| indexitem(val) }
                      if args.empty?
                        rake(opts: opts)
                      else
                        tasks = raketasks
                        while (n, pre = indexitem(args.shift))
                          if (item = tasks[n - 1])
                            cmd = pre ? "#{pre} #{item.first}" : item.first
                          elsif exception
                            indexerror n, tasks
                          else
                            log.warn "rake task #{n} of #{tasks.size} (out of range)"
                            next
                          end
                          if opts.empty?
                            rake cmd
                          else
                            rake(cmd + shell_escape("[#{opts.join(',')}]"))
                            opts.clear
                          end
                        end
                      end
                    end
                  end
                when 'irb'
                  format_desc action, nil, 'opts*,args*|:'
                  task action do |_, args|
                    args = args.to_a
                    name = gemlib.any? { |file| basepath(file).join("#{gemname}.rb").exist? } ? gemname : nil
                    irb(name, args, args: (readline('Enter file [arguments]', force: false) if args.delete(':')))
                  end
                else
                  format_desc(action, nil, 'opts*', before: case action
                                                            when 'cache', 'check' then nil
                                                            else 'command+'
                                                            end)
                  task action do |_, args|
                    bundle(action, *args.to_a)
                  end
                end
              else
                namespace action do
                  flags.each do |flag|
                    case action
                    when 'install', 'update', 'outdated'
                      format_desc action, flag, 'opts*'
                      task flag do |_, args|
                        __send__ action, flag, args.to_a
                      end
                    when 'gem'
                      case flag
                      when :outdated
                        format_desc action, flag, 'major|minor|patch|interactive?,opts*'
                        task flag, [:semver] do |_, args|
                          case (filter = args.semver)
                          when 'major', 'minor', 'patch', 'interactive', 'i'
                            filter = 'interactive' if filter == 'i'
                            args = args.extras
                          else
                            filter = nil
                            args = args.to_a
                          end
                          gem!(flag, args, filter: filter)
                        end
                      when :build, :push, :exec, :update
                        format_desc(action, flag, 'opts*', after: case flag
                                                                  when :exec then 'command,args*'
                                                                  when :push then 'file?|:'
                                                                  when :update then 'name*'
                                                                  end)
                        task flag do |_, args|
                          gem! flag, args.to_a
                        end
                      else
                        format_desc(action, flag, 'opts*', after: flag == :pristine ? 'name*|name?@version' : 'name*')
                        task flag do |_, args|
                          args = param_guard(action, flag, args: args.to_a)
                          gem! flag, args
                        end
                      end
                    when 'ruby'
                      case flag
                      when :file
                        format_desc action, flag, 'path,opts*,args*'
                        task flag, [:rb] do |_, args|
                          file = args.rb
                          opts = args.extras
                          args = if file && !file.include?('*')
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
                                 end
                          ruby(flag, opts, file: file, args: args)
                        end
                      when :script
                        format_desc action, flag, 'opts*'
                        task flag do |_, args|
                          command = ENV['RUBY_E'] || readline('Enter script', force: true, multiline: ['##', ';'])
                          ruby(flag, args.to_a, command: command)
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
            if prod? && !config_get('without')
              if RUBY_VERSION > '3'
                config_set 'without', 'development'
              else
                cmd << '--without=development'
              end
            end
            if (n = option('jobs')).to_i > 0
              cmd << "-j#{n}"
            end
            run_rb(from: :depend, sync: sync)
          end
        end

        def copy(from: gemlib, into: gemdir, override: false, **kwargs)
          return if @copy == false

          glob = kwargs[:include]
          pass = kwargs[:exclude]
          if @copy && !override
            return super unless @copy.is_a?(Hash)

            from = @copy[:from] if @copy.key?(:from)
            glob = @copy[:include] if @copy.key?(:include)
            pass = @copy[:exclude] if @copy.key?(:exclude)
            into = @copy[:into] if @copy.key?(:into)
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
            begin
              copy_dir(a, b, c, pass: pass, verbose: verbosetype > 0)
            rescue StandardError => e
              on_error e, :copy
            end
          end
          on :last, :copy
        end

        def outdated(flag = nil, opts = [], sync: invoked_sync?('outdated', flag))
          cmd = bundle_output 'outdated'
          if flag
            cmd << "--#{flag}"
            append_bundle(opts, OPT_BUNDLE[:outdated] + OPT_BUNDLE[:common], target: cmd)
          end
          log.info cmd.to_s
          on :first, :outdated
          banner = format_banner cmd.to_s
          print_item banner if sync
          pwd_set(from: :outdated) do
            start = 0
            found = 0
            major = 0
            buffer = []
            out = ->(val) { sync ? puts(val) : buffer << val }
            IO.popen(cmd.temp('--no-color')).each do |line|
              if start > 0
                unless stdin?
                  data = line.scan(SEM_VER)
                  next unless (cur = data.shift) && (lat = data.shift)

                  semver cur
                  semver lat
                  c = cur.join
                  l = lat.join
                  styles = []
                  major_set = lambda do
                    styles = %i[green bold]
                    major += 1
                  end
                  minor_set = -> { styles[0] = cur[2] == lat[2] ? :yellow : :green }
                  if data.empty?
                    semmajor?(cur, lat) ? major_set.call : minor_set.call
                  else
                    data.each do |val|
                      break unless line =~ /(>=?|=|~>|!=|<=?) (#{Regexp.escape(val.join)})/

                      v = semver(val).join
                      case $1
                      when '>', '>='
                        semmajor?(cur, lat) ? major_set.call : minor_set.call
                      when '<', '<='
                        if c <= v
                          if semmajor?(cur, lat)
                            major_set.call
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
                  unless styles.empty?
                    case styles.first
                    when :green
                      line = sub_style(line, pat: /^(\S+)(.+)$/, styles: theme[styles.last == :bold ? :major : :active])
                      found += 1
                    when :yellow
                      found += 1
                    end
                    if theme[:current]
                      line = sub_style(line, styles: theme[:current], pat: /^(.+)(#{Regexp.escape(c)})(.+)$/, index: 2)
                    end
                    line = sub_style(line, *colormap(styles), pat: /^((?:\S+\s+){2})(#{Regexp.escape(l)})(.*)$/,
                                                              index: 2)
                  end
                end
                out.call('%2d. %s' % [start, line])
              elsif line.start_with?('Gem')
                unless stdin?
                  sub = { pat: /^(.+)(?<!\dm)(Gem|Latest)(.+)$/, styles: theme[:header], index: 2 }
                  out.call(print_footer(" #  #{line.chomp}", reverse: true, sub: [sub, sub]))
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
              begin
                if major == 0 && dependfile.read =~ /\b(?:source\s+(["'])((?~\1))\1|remote:\s+(\S+))/
                  status = ($2 || $3).chomp('/')
                  right = true
                end
              rescue StandardError => e
                log.debug e
              ensure
                status ||= 'Updates are available'
              end
              puts print_footer(empty_status(status, 'major', major, always: !right), right: right)
            elsif start == 0
              puts 'No updates were found'
            end
          end
          on :last, :outdated
        end

        def install(flag, opts = [])
          bundle_session 'install', "--#{flag}"
          op = append_bundle opts, OPT_BUNDLE[:install_base] + OPT_BUNDLE[:install] + OPT_BUNDLE[:common]
          if op.arg?('force')
            op.delete('--force')
            if flag != :redownload
              op << '--redownload'
            elsif (lock = basepath('Gemfile.lock')).exist?
              config = basepath '.bundle', 'config'
              lock.delete unless config.exist? && config.read.match?(/\bBUNDLE_FROZEN:\s+"true"/)
            end
          end
          run_rb(from: :install)
        end

        def update(flag, opts = [])
          bundle_session 'update', "--#{flag}"
          append_bundle(opts, OPT_BUNDLE[:install_base] + OPT_BUNDLE[:update] + OPT_BUNDLE[:common],
                        append: flag == :all ? nil : /\A[a-z-]+=/)
          run_rb(from: :update)
        end

        def ruby(flag, opts = [], file: nil, command: nil, args: nil)
          case flag
          when :file, :script
            op = OptionPartition.new(opts, OPT_RUBY[:ruby], ruby_session, project: self, args: true)
            if command
              op << quote_option('e', command, option: false)
            elsif file
              op.unshift(basepath(file))
            end
            unless op.arg?('e')
              op.push(args) if args
              op.append(delim: true, escape: false, quote: false) unless op.empty?
            end
          when :version
            pwd_set do
              out = []
              order = { 'rbenv' => -1, 'rvm' => -1, 'asdf' => -1, 'chruby' => -1 }
              ENV.fetch('PATH', '').split(':').each_with_index do |val, index|
                order.each_key do |key|
                  if val.match?(%r{[/.]#{key}/})
                    order[key] = index
                    break
                  end
                end
              end
              paths = [
                "#{ENV.fetch('RBENV_ROOT', '$HOME/.rbenv')}/bin/rbenv",
                '$HOME/.rvm/bin/rvm',
                @asdf ? "#{ENV.fetch('ASDF_DATA_DIR', '$HOME/.asdf')}/installs/#{@asdf.first}" : nil,
                '/usr/bin/rbenv',
                '/usr/local/rvm/bin/rvm',
                '/usr/share/rvm/bin/rvm',
                '/usr/local/share/chruby/chruby.sh'
              ].compact
              paths.sort do |a, b|
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
              .each do |val|
                next unless val.empty? || File.exist?(val.sub('$HOME', Dir.home))

                trim = ->(s) { s[/\A\D+\d+\.\d+(?:\.\S+)?/, 0].sub(/\A([a-z]+)-/i, '\1 ') }
                ver = '.ruby-version'
                out << trim.call(case (cmd = File.basename(val))
                                 when 'rvm'
                                   `rvm current`[/^\S+/, 0]
                                 when 'rbenv'
                                   `rbenv version-name`.yield_self do |name|
                                     name.match?(SEM_VER) ? "ruby #{name}" : name
                                   end
                                 when 'chruby.sh'
                                   chruby = session_output 'source', val
                                   `#{chruby.with('ruby --version')}`
                                 else
                                   if @asdf
                                     cmd = 'asdf'
                                     ver = '.tool-versions'
                                     opt = [@asdf.first]
                                     opt.unshift('--no-header') unless @@asdf[1] == 15
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
              puts(out.map! { |line| '%*s %s' % [pad, line.first, line[1..-1].join(' ')] })
            end
            return
          end
          run_rb(banner: false, from: :"ruby:#{flag}")
        end

        def gem!(flag, opts = [], filter: nil)
          cmd = gem_session
          case flag
          when :outdated
            cmd << gempwd << flag
          else
            cmd << flag
          end
          list = OPT_GEM[flag] + OPT_GEM[:common]
          from = :"gem:#{flag}"
          case flag
          when :install, :update
            list.concat(OPT_GEM[:install_base])
          end
          op = OptionPartition.new(opts, list, cmd, project: self, no: OPT_GEM[:no][flag])
          op.each do |opt|
            if !opt.match?(/\A[A-Za-z\d][A-Za-z\d_.-]*\z/) && %i[install uninstall update pristine].include?(flag)
              op.errors << opt
            else
              op.found << opt
            end
          end
          op.swap
          case flag
          when :outdated
            op.clear
            cmd = cmd.done
            log.info cmd
            on :first, from
            print_item format_banner(cmd)
            major = 0
            minor = 0
            patch = 0
            update = []
            pwd_set(pass: !gempwd.nil?, from: from) do
              items = [[%w[Gem Current Latest], nil]]
              IO.popen(cmd).each do |line|
                if line =~ /^(\S+) \((\S+) < ([^)]+)\)$/
                  cur = semscan $2
                  lat = semscan $3
                  items << [$~.to_a.drop(1), if semmajor?(cur, lat)
                                               1
                                             else
                                               cur[2] == lat[2] ? 3 : 2
                                             end]
                else
                  puts line
                end
              end
              if items.size > 1
                pad = [items.size.to_s.size + 1, 3].max
                d = 0
                e = 0
                f = 0
                j = 0
                queue = nil
                items.each do |item|
                  a, b, c = item.first
                  d = a.size if a.size > d
                  e = b.size if b.size > e
                  f = c.size if c.size > f
                end
                items.each_with_index do |item, i|
                  next if i == 0 && stdin?

                  a, b, c = item.first
                  if i == 0
                    line = '%-*s %-*s    %*s  %*s' % [pad, ' #', d, a, e, b, f, c]
                    n = line.size
                    2.times do
                      line = sub_style(line, pat: /^(.+)(?<!\dm)(#{a}|#{c})(.*)$/, styles: theme[:header], index: 2)
                    end
                    queue = [line, sub_style(ARG[:BORDER][1] * n, styles: borderstyle)]
                  else
                    g = a.ljust(d)
                    pat = [/^([^.]+\.)([^.]+\..+)$/, /^([^.]+\.[^.]+\.)(.+)$/]
                    pre = b.start_with?('0.')
                    latest = [theme[:latest]]
                    case item.last
                    when 1
                      case filter
                      when 'major'
                        update << a
                      when 'minor', 'patch'
                        next
                      end
                      g = sub_style(g, styles: theme[:major])
                      major += 1
                      styles = %i[green bold]
                      pat = pre ? pat.first : nil
                      latest << :bold
                    when 2
                      case filter
                      when 'major', 'minor'
                        update << a
                      when 'patch'
                        next
                      end
                      g = sub_style(g, styles: theme[:active])
                      minor += 1
                      styles = %i[green]
                      pat = pre ? pat.last : pat.first
                    else
                      case filter
                      when 'major', 'minor', 'patch'
                        update << a
                      end
                      patch += 1
                      styles = %i[yellow]
                      pat = pat.last
                    end
                    b = sub_style(b.rjust(e), *colormap(styles), pat: pat, index: 2)
                    h = sub_style(c.rjust(f), styles: latest.flatten.compact, pat: pat, index: 2)
                    j += 1
                    if queue
                      puts queue
                      queue = nil
                    end
                    puts '%*s %s    %s  %s' % [pad, "#{j}.", g, b, h]
                    update << a if filter == 'interactive' && confirm_outdated(a, c, item.last)
                  end
                end
              end
            end
            if major + minor + patch == 0
              puts 'No updates were found'
            else
              unless update.empty?
                cmd = gem_output 'update', '-f'
                option('document', prefix: 'gem', ignore: false) do |val|
                  cmd << case val
                         when '0', 'false'
                           '--no-document'
                         else
                           basic_option 'document', val
                         end
                end
                option('user-install', prefix: 'gem', ignore: false) do |val|
                  cmd << case val
                         when '0', 'false'
                           '--no-user-install'
                         else
                           '--user-install'
                         end
                end
                cmd.merge(update)
                run(cmd, banner: false, from: :'gem:update')
              end
              print_status(major, minor, patch, from: :outdated)
            end
            on :last, from
            return
          when :build
            if op.empty?
              raise_error('gemspec not found', hint: project) unless gemfile
              op.add_path(gemfile)
            else
              op.add_path(op.shift)
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
              file = op.shift.yield_self { |val| val.include?('.') ? val : "#{val}.gem" }
              raise_error("unknown args: #{op.join(', ')}", hint: flag) unless op.empty?
            end
            raise_error('gem not found', hint: file) unless op.exist?(file)
            op.add_path(file)
            run_rb(from: from, interactive: "Push #{sub_style(gemname, styles: theme[:active])}")
            return
          when :exec
            min = if op.arg?('g', 'gem')
                    1
                  elsif op.empty?
                    op << basic_option('gem', gemname)
                    1
                  else
                    op << op.shift
                    0
                  end
            if (args = command_args(op.extras, min: min, force: min == 1 && op.empty?))
              op.push(args)
            end
            op.append(quote: false)
          when :update
            unless op.arg?('system')
              if op.empty?
                op << gemname
              else
                op.append
              end
            end
            op.clear(errors: true)
          when :install, :uninstall, :pristine
            raise_error('missing gemname', hint: flag) if op.empty?
            if op.arg?('all')
              if flag == :pristine
                append_repeat 'skip', op.extras
                op.reset
              else
                op.clear
              end
            elsif (n = op.index { |val| val.match?(/(\A|[a-z])@\d/) })
              name = op.delete_at(n)
              pre, ver = if (n = name.index('@')) == 0
                           [gemname, name[1..-1]]
                         else
                           [name[0, n], name[(n + 1)..-1]]
                         end
              op.adjoin(pre, basic_option('version', ver))
                .clear
            elsif flag == :install
              op.append_any
            else
              op.append
            end
            op.clear(errors: true)
            op.delim << readline('Enter command [args]', force: true) if flag == :install && op.remove(':')
          else
            op.append
          end
          run_rb(from: from)
        end

        def bundle(flag, *args)
          cmd = bundle_session flag
          args = case flag
                 when 'exec', 'cache', 'check'
                   list = OPT_BUNDLE[flag.to_sym] + OPT_BUNDLE[:common]
                   OptionPartition.new(args, list, cmd, project: self, args: flag == 'exec').extras
                 else
                   args.flatten
                 end
          case flag
          when 'exec', 'config'
            cmd << readline('Enter arguments', force: true) if args.empty?
          when 'cache', 'check'
            option_clear args
            args.clear
          end
          cmd.merge(args)
          run(from: :"bundle:#{flag}")
        end

        def rake(*args, opts: [])
          op = OptionPartition.new(opts, OPT_RUBY[:rake], [(quote_option('f', rakefile) if rakefile)].compact,
                                   project: self)
          args.concat(op.extras)
          if args.empty?
            args << nil
          else
            args.flatten!
          end
          cmd = rake_output(*op.to_a)
          args.map! { |val| cmd.temp(val) }
          run_s(args, banner: false, from: :rake)
        end

        def irb(name = nil, opts = [], path: gemlib, args: nil)
          op = OptionPartition.new(opts, OPT_RUBY[:irb], session('irb'), project: self, first: [/\.rb$/])
          r = args ? [] : ['bundler/setup']
          r << name if name
          r.each { |val| op << shell_option('r', val, merge: true) }
          Array(path).each { |val| op << quote_option('I', val, merge: true) }
          if args
            op.delim << args
            op.clear
          else
            op.append(delim: true)
          end
          run(banner: false)
        end

        def gemspec
          @gemspec = !gemfile.nil? && Gem::Specification.load(gemfile.to_s) rescue false if @gemspec.nil?
          @gemspec || nil
        end

        def gemname
          @gemname ||= ((spec = gemspec) ? spec.name : project)
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
            return false unless base.join(gempath(val, 'specifications')).exist?

            log.warn "using version #{val} (given #{version})" if version && version != val
            self.version = val
            self.gemdir = base + gempath
          end
          if version
            begin
              case @autodetect
              when 'rvm'
                self.gemdir = pwd_set { `rvm info homes` }[/^\s+gem:\s+"(.+)"$/, 1]
              when 'rbenv'
                if pwd_set { `rbenv which ruby` } =~ %r{^(.+[\\/]versions[\\/](\d\.\d)\.[^\\/]+)[\\/]bin[\\/]ruby$}
                  self.gemdir = File.join($1, 'lib/ruby/gems', "#{$2}.0")
                end
              when 'asdf'
                val = pwd_set { `asdf where ruby`.chomp }
                self.gemdir = File.join(val, 'lib/ruby/gems', "#{$1}.0") if val =~ /(\d\.\d)\.[^.]+$/
              when /bundler?/
                path = pwd_set { `bundle env` }[/^\s+Gem Path\s+(.+)$/, 1]
                self.gemdir = path.split(File::PATH_SEPARATOR).find { |val| Dir.exist?(val) }
              else
                self.gemdir = ENV['GEM_HOME'] || ENV['GEM_ROOT']
              end
              return true if gemdir?
            rescue StandardError => e
              log.debug e
            end
            pwd_set(pass: !gempwd.nil?) do
              out = `#{gem_output(gempwd, 'list --local -d', gemname)}`
              if out =~ /#{Regexp.escape(gemname)}\s+\((.+)\)$/
                split_escape($1)
                  .unshift(version)
                  .uniq
                  .each do |val|
                    next unless out =~ /(?:\(#{Regexp.escape(val)}[^)]*\)|Installed at):\s+(.+)$/

                    return gemdir? if set.call(val, $1)
                  end
              end
            end
            self.gemdir = Pathname.new(Gem.dir) + gempath
          else
            parse = lambda do |path|
              next unless path

              lib = Regexp.new(['', 'gems', "#{gemname}-([^#{File::SEPARATOR}]+)", ''].join(File::SEPARATOR))
              if (ver = path[lib, 1]) && (val = path[/\A(.+)#{Regexp.escape(gempath(ver))}/, 1])
                set.call(ver, val)
              end
            end
            if RUBY_VERSION >= '2.6'
              target = RUBY_VERSION.start_with?('2.6') ? RubyVM : $LOAD_PATH
              parse.call(target.resolve_feature_path(gemname)&.last)
            end
            if !gemdir && !pwd_set { parse.call(`#{bundle_output('show', gemname)}`) }
              raise_error 'gems directory not found'
            end
          end
        rescue StandardError => e
          log.error e
          @version = nil
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

        def append_bundle(opts, list, target: @session, append: nil)
          op = OptionPartition.new(opts, list, target, project: self)
          if append
            if append.is_a?(Regexp)
              op.each do |opt|
                if opt.match?(append)
                  op.errors << opt
                else
                  op.found << opt
                end
              end
              op.swap.clear(errors: true)
            end
            op.append(escape: true)
          else
            op.clear
          end
          op
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

        def config_set(key, *val)
          run(bundle_output('config set', key, *val), banner: false, series: false)
        end

        def preopts
          verbosetype > 1 ? ['--verbose'] : []
        end

        def variables
          (super + %i[autodetect]).freeze
        end

        def rakefile
          if @rakefile.nil?
            file = Rake::Application::DEFAULT_RAKEFILES.find { |val| basepath(val).exist? }
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

        def gempwd
          return unless !pwd? && semgte?(Gem::VERSION, '3.4.2')

          quote_option 'C', path
        end

        def gemfile
          if @gemfile.nil?
            @gemfile = [project, name].map! { |val| basepath("#{val}.gemspec") }
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
            lib.select { |file| basepath(file).exist? }
          end
        end

        def gempath(val = version, dir = 'gems')
          ret = File.join(dir, "#{gemname}-#{val}")
          ret += '.gemspec' if dir == 'specifications'
          ret
        end

        def gemdir?
          return false unless gemdir

          gemdir.exist? && !gemdir.empty?
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
            common: %w[all-resources compatibility dry-run ansi|b env-file=p f|file=p parallel=n profile=b progress=b
                       project-directory=p p|project-name=e].freeze,
            build: %w[check no-cache print pull push with-dependencies q|quiet build-arg=qq builder=b m|memory=b
                      provenance=q sbom=q ssh=qq].freeze,
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
                       disable-content-trust=b? dns=q dns-option=q dns-search=q domainname=b entrypoint=q e|env=qq
                       env-file=p expose=q gpus=q group-add=b health-cmd=q health-interval=b health-retries=i
                       health-start-interval=q health-start-period=q health-timeout=q hostname=q io-maxbandwidth=b
                       io-maxiops=b ip=b ip6=q ipc=b isolation=b kernel-memory=b l|label=q label-file=q link=b
                       link-local-ip=q log-driver=b log-opt=q mac-address=q m|memory=b memory-reservation=b
                       memory-swap=n memory-swappiness=n mount=qq name=b network=b network-alias=b oom-score-adj=b
                       pid=b pids-limit=n platform=q p|publish=q pull=b restart=b runtime=b security-opt=q shm-size=b
                       stop-signal=b stop-timeout=i storage-opt=q sysctl=q tmpfs=q ulimit=q u|user=b userns=b uts=b
                       v|volume=q volume-driver=b volumes-from=b w|workdir=q].freeze,
            run: %w[d|detach detach-keys=q sig-proxy=b?].freeze,
            update: %w[blkio-weight=i cpu-period=i cpu-quota=i cpu-rt-period=i cpu-rt-runtime=i c|cpu-shares=i cpus=f
                       cpuset-cpus=b cpuset-mems=b m|memory=b memory-reservation=b memory-swap=b pids-limit=n
                       restart=q].freeze,
            exec: %w[d|detach i|interactive privileged t|tty detach-keys=q e|env=qq env-file=p user=e
                     w|workdir=q].freeze,
            commit: %w[a|author=q c|change=q m|message=q pause=b?].freeze,
            inspect: %w[s|size f|format=q type=b].freeze,
            start: %w[a|attach i|interactive detach-keys=q].freeze,
            stop: %w[s|signal=b t|time=i t|timeout=i].freeze,
            restart: %w[s|signal=b t|time=i t|timeout=i].freeze,
            kill: %w[s|signal=b].freeze,
            stats: %w[a|all no-stream no-trunc format|q].freeze
          }.freeze,
          image: {
            list: %w[a|all q|quiet digests no-trunc tree f|filter=q format=q].freeze,
            push: %w[a|all-tags disable-content-trust=b? platform=q q|quiet].freeze,
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
          'compose' => %i[build run exec up down].freeze,
          'bake' => %i[build check].freeze,
          'image' => %i[list rm push tag save].freeze,
          'container' => %i[run create exec update commit inspect diff start stop restart pause unpause top stats kill
                            rm].freeze,
          'network' => %i[connect disconnect].freeze
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

              namespace action do
                flags.each do |flag|
                  case action
                  when 'build'
                    case flag
                    when :tag, :context
                      format_desc(action, flag, 'opts*', before: flag == :tag ? 'name' : 'dir')
                      task flag, [flag] do |_, args|
                        param = param_guard(action, flag, args: args, key: flag)
                        buildx(:build, args.extras, "#{flag}": param)
                      end
                    end
                  when 'bake'
                    break unless bake?

                    case flag
                    when :build
                      format_desc action, flag, 'opts*,target*,context?|:'
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
                    when :build, :up, :down
                      format_desc action, flag, 'opts*,service*|:'
                      task flag do |_, args|
                        compose! flag, args.to_a
                      end
                    when :exec, :run
                      format_desc action, flag, "service|:,command#{flag == :exec ? '' : '?'}|::,args*,opts*"
                      task flag, [:service] do |_, args|
                        service = param_guard(action, flag, args: args, key: :service)
                        compose!(flag, args.extras, service: service)
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
                                                when :rm, :save then 'id*,opts*'
                                                when :tag then 'version?'
                                                else 'opts*,args*'
                                                end)
                      task flag do |_, args|
                        args = args.to_a
                        if args.empty? && flag != :list
                          choice_command flag
                        else
                          image flag, args
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

        def clean(*, sync: invoked_sync?('clean'), **)
          if runnable?(@clean)
            super
          else
            image(:rm, sync: sync)
          end
        end

        def compose(opts, flags = nil, script: false, args: nil, from: :run, **)
          return opts if script == false

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
          [args, flags].each_with_index do |target, index|
            if (data = append_any(target, target: []))
              ret.merge(data.map { |arg| index == 0 ? fill_option(arg) : quote_option('build-arg', arg) })
            end
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
            option(from == :bake ? 'target' : 'service', ignore: false) do |a|
              ret.merge(split_escape(a).map! { |b| shell_quote(b) })
            end
          end
          ret
        end

        def buildx(flag, opts = [], tag: nil, context: nil)
          cmd, opts = docker_session('buildx', opts: opts)
          op = OptionPartition.new(opts, OPT_DOCKER[:buildx][:common], cmd, project: self)
          op << flag
          op.parse(OPT_DOCKER[:buildx][flag == :bake ? :bake : :build] + OPT_DOCKER[:buildx][:shared])
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

        def compose!(flag, opts = [], service: nil)
          cmd, opts = docker_session('compose', opts: opts)
          op = OptionPartition.new(opts, OPT_DOCKER[:compose][:common], cmd, project: self)
          append_file filetype unless op.arg?('f', 'file')
          op << flag
          op.parse(OPT_DOCKER[:compose].fetch(flag, []))
          multiple = case flag
                     when :build, :up, :down then true
                     else false
                     end
          if op.remove(':') || service == ':'
            keys = Set.new
            read_composefile('services', target: op.values_of('f', 'file')) { |data| keys.merge(data.keys) }
            service = unless keys.empty?
                        choice_index('Add services', keys, multiple: multiple, force: !multiple,
                                                           attempts: multiple ? 1 : 5)
                      end
          end
          if multiple
            op.concat(service) if service
            op.append(delim: true, escape: true, strip: /^:/)
          else
            raise_error('no services were found', hint: flag) unless service
            append_command(flag, service, op.extras, prompt: '::')
          end
          run(from: :"compose:#{flag}")
        end

        def container(flag, opts = [], id: nil)
          cmd, opts = docker_session('container', flag, opts: opts)
          rc = flag == :run || flag == :create
          list = OPT_DOCKER[:container].fetch(flag, [])
          list += OPT_DOCKER[:container][:create] if flag == :run
          list += OPT_DOCKER[:container][:update] if rc
          op = OptionPartition.new(opts, list, cmd, project: self, args: rc || flag == :exec)
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
                      raise_error("unknown type: #{v}", hint: flag)
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
                  elsif verbose
                    log_message(Logger::INFO, 'unrecognized option', subject: from, hint: k)
                  end
                end
                raise_error('missing type', hint: flag) unless type
                cmd << "--mount type=#{type},#{args.join(',')}"
              end
            end
            append_command(flag, id || tagmain, op.extras)
          when :update
            raise_error('missing container', hint: flag) if op.empty?
            op.append(escape: true, strip: /^:/)
          when :commit
            latest = op.shift || tagmain
            cmd << id << latest
            raise_error("unknown args: #{op.join(', ')}", hint: flag) unless op.empty?
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
            opts << '--quiet' unless verbose
            return image(:push, opts, id: latest, registry: registry)
          else
            if op.empty?
              status = []
              no = true
              case flag
              when :inspect, :diff
                no = false
              when :start
                status = %w[created exited]
                no = false
              when :stop, :pause
                status = %w[running restarting]
              when :restart
                status = %w[running paused exited]
              when :unpause
                status << 'paused'
                no = false
              when :top, :stats
                status << 'running'
                cmd << '--no-stream' if flag == :stats
                no = false
              when :kill
                status = %w[running restarting paused]
              when :rm
                status = %w[created exited dead]
              end
              ps = docker_output('ps -a', *status.map { |s| quote_option('filter', "status=#{s}") })
              list_image(flag, ps, no: no, hint: "status: #{status.join(', ')}", from: from) do |img|
                run(cmd.temp(img), from: from)
              end
              return
            else
              op.append(escape: true, strip: /^:/)
            end
          end
          run(from: from)
        end

        def image(flag, opts = [], sync: true, id: nil, registry: nil)
          cmd, opts = docker_session('image', flag, opts: opts)
          op = OptionPartition.new(opts, OPT_DOCKER[:image].fetch(flag, []), cmd, project: self)
          exception = @exception
          banner = true
          from = :"image:#{flag}"
          case flag
          when :list
            if opts.size == op.size
              index = 0
              name = nil
              opts.reverse_each { |opt| break opts.delete(opt) if (name = opt[/^name=["']?(.+?)["']?$/, 1]) }
              list_image(:run, cmd << '-a', from: from) do |val|
                container(:run, if name
                                  opts.dup << "name=#{index == 0 ? name : "#{name}-#{index}"}"
                                else
                                  opts
                                end, id: val)
                index += 1
              end
              return
            else
              op.clear
            end
          when :rm
            if id
              op << id
              if option('y')
                exception = false
                banner = false
              end
            else
              if op.empty?
                list_image(:rm, docker_output('image ls -a'), from: from) do |val|
                  image(:rm, opts, sync: sync, id: val)
                end
              else
                op.each { |val| run(cmd.temp(val), sync: sync, from: from) }
              end
              return
            end
          when :tag, :save
            list_image(flag, docker_output('image ls -a'), from: from) do |val|
              op << val
              if flag == :tag
                op << tagname("#{project}:#{op.first}")
                break
              end
            end
          when :push
            id ||= option('tag', ignore: false) || tagmain
            registry ||= op.shift || option('registry') || @registry
            raise_error(id ? "unknown args: #{op.join(', ')}" : 'no id/tag given', hint: flag) unless id && op.empty?
            raise_error('username/registry not provided', hint: flag) unless registry
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
          ret = run(cmd, sync: sync, exception: exception, banner: banner, from: from)
          print_success if success?(ret, flag == :tag || flag == :save)
        end

        def network(flag, opts = [], target: nil)
          cmd, opts = docker_session('network', flag, opts: opts)
          op = OptionPartition.new(opts, OPT_DOCKER[:network].fetch(flag, []), cmd, project: self)
          op.clear
          from = :"network:#{flag}"
          list_image(flag, docker_output('ps -a'), from: from) do |img|
            print_success if success?(run(cmd.temp(target, img), from: from))
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
                      val = val.select { |file| basepath(file).exist? }
                      val.size > 1 ? val : val.first
                    elsif val == true
                      DIR_DOCKER.find { |file| basepath(file).exist? }
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
          ret = session('docker', *op.to_a, *cmd)
          [ret, op.extras]
        end

        def docker_output(*cmd, **kwargs)
          session('docker', *cmd, main: false, options: false, **kwargs)
        end

        def append_command(flag, val, list, target: @session, prompt: ':')
          if list.delete(prompt)
            list << readline('Enter command [args]', force: flag == :exec)
          elsif (args = env('DOCKER_ARGS'))
            list << args
          end
          case flag
          when :run
            unless session_arg?('name', target: target)
              target << basic_option('name', dnsname("#{name}_%s" % rand_s(6)))
            end
          when :exec
            raise_error('no command args', hint: flag) if list.empty?
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
              return unless COMPOSEFILE.select { |val| basepath(val).exist? }.size > 1
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
          case val
          when String
            split_escape val
          when Array
            val
          else
            []
          end.yield_self do |list|
            ver = option('version', target: target, ignore: false)
            list.each do |s|
              s = "#{s}:#{ver}" if ver && (!s.include?(':') || s.delete_suffix!(':latest'))
              target << basic_option('tag', tagname(s))
            end
            target
          end
        end

        def list_image(flag, cmd, hint: nil, no: true, from: nil)
          pwd_set(from: from) do
            found = false
            index = 0
            all = option('all', prefix: 'docker')
            y = from == :'image:rm' && option('y', prefix: 'docker')
            pat = /\b(?:#{dnsname(name)}|#{tagname(project)}|#{tagmain.split(':', 2).first})\b/
            IO.popen(session_done(cmd << '--format=json')).each do |line|
              data = JSON.parse(line)
              id = data['ID']
              rt = [data['Repository'], data['Tag']].reject { |val| val == '<none>' }.join(':')
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
                bb = index.succ.to_s
                cc = bb.size + 1
                a = sub_style(ee, styles: theme[:inline])
                b = "Execute #{sub_style(flag, styles: theme[:active])} on #{a}#{ee == id ? '' : " (#{id})"}"
                e = time_format(time_since(data['CreatedAt']), pass: ['ms'])
                f = sub_style(ARG[:BORDER][0], styles: theme[:inline])
                g = ' ' * (cc + 1)
                h = "#{sub_style(bb.rjust(cc), styles: theme[:current])} #{f} "
                puts unless index == 0
                puts "#{h + sub_style(aa, styles: theme[:subject])} (created #{e} ago)"
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
                puts g + sub_style(ARG[:BORDER][6] + (ARG[:BORDER][1] * w), styles: theme[:inline])
                found = true
                index += 1
                next unless confirm("#{h + b}?", no ? 'N' : 'Y', timeout: 60)

                puts if printfirst?
              end
              yield id
            end
            puts log_message(Logger::INFO, 'none detected', subject: name, hint: hint || from) if !found && !y
          end
        rescue StandardError => e
          on_error e, from
        end

        def confirm_command(*args, title: nil, target: nil, as: nil)
          return false unless title && target

          puts unless printfirst?
          t = title.to_s.split(':')
          emphasize(args, title: message(t.first.upcase, *t.drop(1)), border: borderstyle, sub: [
            { pat: /\A(\w+(?: => \w+)+)(.*)\z/, styles: theme[:header] },
            { pat: /\A(.+)\z/, styles: theme[:caution] }
          ])
          printsucc
          a = t.last.capitalize
          b = sub_style(target, styles: theme[:subject])
          c = as && sub_style(as, styles: theme[:inline])
          confirm("#{a} #{b}#{c ? " as #{c}" : ''}?", 'N', timeout: 60)
        end

        def choice_command(flag)
          msg, cmd, index = case flag
                            when :exec
                              ['Choose a container', 'ps -a', 0]
                            when :bake
                              ['Choose a target', 'buildx bake --list=type=targets', 0]
                            when :connect, :disconnect
                              ['Choose a network', 'network ls', 0]
                            else
                              ['Choose an image', 'images -a', 2]
                            end
          lines = `#{docker_output(cmd)}`.lines
          header = lines.shift
          if lines.empty?
            puts log_message(Logger::INFO, 'none found', subject: name, hint: "docker #{cmd}")
          else
            puts " #  #{header}"
            multiple = false
            parse = ->(val) { val.split(/\s+/)[index] }
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
            when :connect, :disconnect
              values = ['Options', ['Container', true]]
              ctx = "network #{flag}"
            end
            out, opts, args = choice_index(msg, lines, multiple: multiple, values: values)
            cmd = docker_output ctx
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
            cmd.merge(Array(out).map! { |val| parse.call(val) })
            cmd << args
            print_success if success?(run(cmd), ctx.start_with?(/network|tag|save/))
          end
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
          args.compact!
          args.join(char) unless args.empty?
        end

        def tagname(val)
          val = val.split(':').map! { |s| charname(s.sub(/^\W+/, '')) }
          val.join(':').yield_self do |s|
            s = val.first if val.size > 1 && s.size > 128
            s[0..127]
          end
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
                      project ? [project, 'not found'] : %w[name missing]
                    end
        warn log_message(Logger::WARN, msg, subject: self.class, hint: hint)
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
                target = file || target? ? file || realpath : nil

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
          raise_error(file, mime, hint: 'invalid') unless ext.include?(mime)
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
            args ||= { hint: 'not found', kind: LoadError }
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

        title = Pathname.new(file)
                        .realpath
                        .to_s
                        .sub(/\A#{Regexp.escape(File.join(Dir.pwd, ''))}/, '')
        emphasize(lines, title: title, sub: unless stdin?
                                              [
                                                { pat: /\A((?:[^:]|(?<! ):(?! ))+)\z/, styles: theme[:banner] },
                                                { pat: /\A(.*?)(<[^>]+>)(.+)\z/m, styles: theme[:undefined], index: 2 },
                                                { pat: /\A((?~ : ))( : (?!undefined).+)\z/m, styles: theme[:key] },
                                                { pat: /\A((?~: ): )(-?[\d.]+)(\s*)\z/m, styles: theme[:number],
                                                  index: 2 },
                                                { pat: /\A((?~: ): ")(.+)("\s*)\z/m, styles: theme[:string], index: 2 },
                                                { pat: /\A((?~: ): \{)(.+)(}\s*)\z/m, styles: theme[:hash], index: 2 },
                                                { pat: /\A((?~: ): \[)(.+)(\]\s*)\z/m, styles: theme[:array],
                                                  index: 2 },
                                                { pat: /\A((?~: ): )(true|false)(\s*)\z/m, styles: theme[:boolean],
                                                  index: 2 },
                                                { pat: /\A((?~: ): (?!undefined))([^"\[{].*)\z/m, styles: theme[:value],
                                                  index: 2 }
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
            log&.warn "#{Viewer}(#{type}) => #{file && "#{file} "}{#{key}: undefined}"
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

        val = "#{ext.first}[#{target ? '' : "file?=#{File.basename(main)}.#{ext.last},"}keys+]"
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
      add('publish/*', group: 'emc', pass: %w[install update package], exclude: :base)
      revbuild(include: 'src/')
      inject(Viewer, dump: 'json')

      chain('all', :refresh, step: 1)
    end
    add('pi-r', 'pir', graph: 'emc', copy: { from: 'publish', scope: '@pi-r', also: :pir2 }) do
      add('publish/*', group: 'pir', pass: %w[install update package], exclude: :base)
      revbuild(include: 'src/')
      inject(Viewer, dump: 'json')

      chain('all', :refresh, after: 'emc')
    end
    add('pi-r2', 'pir2', graph: %w[pir emc], copy: { from: 'publish', workspace: true }) do
      add('publish/*', group: 'pir2', pass: %w[install update package], exclude: :base)
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
  .with(:docker, run: (false unless ENV['SSH_AUTH_SOCK'] && ENV['GITHUB_TOKEN']), pass: %i[windows? docker?]) do
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
