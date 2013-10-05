#!/usr/bin/ruby

require 'rubygems'
require 'redcarpet'
require 'albino'

class SyntaxRenderer < Redcarpet::Render::HTML
  def block_code(code, language)
    if language && !language.empty?
      Albino.colorize(code, language)
    else
      "<pre><code>#{code}</code></pre>"
    end
  end
end

def markdown(text)
  renderer = SyntaxRenderer.new(optionize [
    :with_toc_data,
    #:hard_wrap,
    :xhtml
  ])
  markdown = Redcarpet::Markdown.new(renderer, optionize([
    :fenced_code_blocks,
    :no_intra_emphasis,
    :tables,
    :superscript,
    :autolink,
    :strikethrough,
    :space_after_headers,
    :with_toc_data,
    #:no_styles,
    :lax_spacing
  ]))
  markdown.render(text)
end

def optionize(options)
  options.inject({}) { |memo, option| memo[option] = true; memo }
end

puts markdown(ARGF.read)