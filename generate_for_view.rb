#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
require 'pathname'
require 'fileutils'
require 'tempfile'
require 'tmpdir'
require 'nokogiri'
require 'fssm'
require 'pry'
require 'colorize'

class Pathname
  def change_ext(ext)
    self.dirname + (self.basename.to_s[/^(.+)\.([^\.]+)$/,1] + '.' + ext)
  end
end

module Altech; end

module Altech::Renderer
  module_function
  extend FileUtils
  extend self

  CSS = DATA.read
  
  def from_markdown_to_html(src,dest)
    tf = Tempfile.new(['conv','.html'])
    `pandoc -s #{src.realpath} -o #{tf.path}`
    File.write(dest, add_css(tf.read))
    tf.close
  end

  def from_org_to_html(src,dest)
    Dir.mktmpdir do |tmpdir|
      `emacs -q --batch --visit=#{src} --execute='(org-export-as-html 3 nil nil nil nil "#{tmpdir}")'`
      File.write(dest, add_css(File.read(File.expand_path(src.basename.change_ext('html').to_s, tmpdir))))
    end
  end

  def to_html(entry, src_dir, dest_dir)
    src_dir, dest_dir = [src_dir, dest_dir].map{|dir| (dir+entry).dirname}
    src_entry  = Pathname.new(entry).basename
    dest_entry = Pathname.new(map src_entry)

    dest_dir.mkdir unless dest_dir.exist?

    type, _ = LIST.find{|type,ext| ext.include? File.extname(src_entry)[1..-1]}
    if type
      self.send("from_#{type}_to_html".to_sym, src_dir+src_entry,dest_dir + dest_entry);
      true
    else
      FileUtils.cp (src_dir+src_entry), (dest_dir+dest_entry)
      false
    end
  end

  LIST = {
    org: %w[org],
    markdown: %w[markdown md mkd]
  }

  def render(src_dir,dest_dir, continuous = false)
    src_dir, dest_dir = Pathname.new(File.expand_path(src_dir)), Pathname.new(File.expand_path(dest_dir))
    unless src_dir.exist? and dest_dir.exist?
      puts "The directory is not found."
      return
    end

    rm_r dest_dir; mkdir dest_dir

    Dir.glob(src_dir + '**/*').select{|p| File.file?(p)}.each do |path|
      next unless filter path
      path = Pathname.new(path)
      path.dirname.mkdir unless path.dirname.exist?
      to_html(path.relative_path_from(src_dir), src_dir, dest_dir)
    end

    if continuous
      system 'clear'

      begin
        FSSM.monitor(src_dir.to_s, '**/*') do
          update_or_create = ->(file) do
            if Altech::Renderer.to_html(file,src_dir,dest_dir)
              puts "$ #{file} was rendered.".green
            else
              puts "$ #{file} was not rendered.".red
            end
          end
          filter = ->(file){ Altech::Renderer.filter file }
          
          update do |_, file|
            update_or_create[file] if filter[file]
          end
          create do |_, file|
            update_or_create[file] if filter[file]
          end
          delete do |_, file|
            if filter[file]
              _dest_dir, _file = (dest_dir + file).dirname, Pathname.new(file).basename
              (_dest_dir + Altech::Renderer.map(_file)).delete
              puts "$ #{_file} was deleted.".yellow
              if _dest_dir.entries.select{|p| not p.to_s =~ /^\./}.size == 0
                _dest_dir.rmdir
                puts "$ #{_dest_dir}(directory) was deleted.".yellow
              end
            end
          end
        end
      rescue => e
        puts "Error #{e.inspect}"
        retry
        # if Time.now - prev_error_time < 10
          # raise e
        # else
        #   retry
        # end
        # retry 
      end
    end
  end

  def filter(file)
    !(Pathname.new(file).basename.to_s =~ /^[#\.]/)
  end

  # map of file names from source file to destination file
  def map(file)
    file.dirname + (file.basename.to_s[/^[^\._]+/] + '.html')
  end

  private

  def add_css(text)
    doc = Nokogiri::HTML(text)
    doc.traverse do |x|
      if x.name == 'style'
        x << CSS
      end
    end
    return doc.to_s
  end

end

Altech::Renderer.render '~/Dropbox/Notes', '~/Dropbox/RNotes', true


__END__

body {
  font-family: Helvetica, arial, sans-serif;
  font-size: 14px;
  line-height: 1.6;
  padding-top: 10px;
  padding-bottom: 10px;
  background-color: white;
  padding: 30px; }

body > *:first-child {
  margin-top: 0 !important; }
body > *:last-child {
  margin-bottom: 0 !important; }

a {
  color: #4183C4; }
a.absent {
  color: #cc0000; }
a.anchor {
  display: block;
  padding-left: 30px;
  margin-left: -30px;
  cursor: pointer;
  position: absolute;
  top: 0;
  left: 0;
  bottom: 0; }

h1, h2, h3, h4, h5, h6 {
  margin: 20px 0 10px;
  padding: 0;
  font-weight: bold;
  -webkit-font-smoothing: antialiased;
  cursor: text;
  position: relative; }

h1:hover a.anchor, h2:hover a.anchor, h3:hover a.anchor, h4:hover a.anchor, h5:hover a.anchor, h6:hover a.anchor {
  background: url("../../images/modules/styleguide/para.png") no-repeat 10px center;
  text-decoration: none; }

h1 tt, h1 code {
  font-size: inherit; }

h2 tt, h2 code {
  font-size: inherit; }

h3 tt, h3 code {
  font-size: inherit; }

h4 tt, h4 code {
  font-size: inherit; }

h5 tt, h5 code {
  font-size: inherit; }

h6 tt, h6 code {
  font-size: inherit; }

h1 {
  font-size: 28px;
  color: black; }

h2 {
  font-size: 24px;
  border-bottom: 1px solid #cccccc;
  color: black; }

h3 {
  font-size: 18px; }

h4 {
  font-size: 16px; }

h5 {
  font-size: 14px; }

h6 {
  color: #777777;
  font-size: 14px; }

p, blockquote, ul, ol, dl, li, table, pre {
  margin: 15px 0; }

hr {
  background: transparent url("../../images/modules/pulls/dirty-shade.png") repeat-x 0 0;
  border: 0 none;
  color: #cccccc;
  height: 4px;
  padding: 0; }

body > h2:first-child {
  margin-top: 0;
  padding-top: 0; }
body > h1:first-child {
  margin-top: 0;
  padding-top: 0; }
  body > h1:first-child + h2 {
    margin-top: 0;
    padding-top: 0; }
body > h3:first-child, body > h4:first-child, body > h5:first-child, body > h6:first-child {
  margin-top: 0;
  padding-top: 0; }

a:first-child h1, a:first-child h2, a:first-child h3, a:first-child h4, a:first-child h5, a:first-child h6 {
  margin-top: 0;
  padding-top: 0; }

h1 p, h2 p, h3 p, h4 p, h5 p, h6 p {
  margin-top: 0; }

li p.first {
  display: inline-block; }

ul, ol {
  padding-left: 30px; }

ul :first-child, ol :first-child {
  margin-top: 0; }

ul :last-child, ol :last-child {
  margin-bottom: 0; }

dl {
  padding: 0; }
  dl dt {
    font-size: 14px;
    font-weight: bold;
    font-style: italic;
    padding: 0;
    margin: 15px 0 5px; }
    dl dt:first-child {
      padding: 0; }
    dl dt > :first-child {
      margin-top: 0; }
    dl dt > :last-child {
      margin-bottom: 0; }
  dl dd {
    margin: 0 0 15px;
    padding: 0 15px; }
    dl dd > :first-child {
      margin-top: 0; }
    dl dd > :last-child {
      margin-bottom: 0; }

blockquote {
  border-left: 4px solid #dddddd;
  padding: 0 15px;
  color: #777777; }
  blockquote > :first-child {
    margin-top: 0; }
  blockquote > :last-child {
    margin-bottom: 0; }

table {
  padding: 0; }
  table tr {
    border-top: 1px solid #cccccc;
    background-color: white;
    margin: 0;
    padding: 0; }
    table tr:nth-child(2n) {
      background-color: #f8f8f8; }
    table tr th {
      font-weight: bold;
      border: 1px solid #cccccc;
      text-align: left;
      margin: 0;
      padding: 6px 13px; }
    table tr td {
      border: 1px solid #cccccc;
      text-align: left;
      margin: 0;
      padding: 6px 13px; }
    table tr th :first-child, table tr td :first-child {
      margin-top: 0; }
    table tr th :last-child, table tr td :last-child {
      margin-bottom: 0; }

img {
  max-width: 100%; }

span.frame {
  display: block;
  overflow: hidden; }
  span.frame > span {
    border: 1px solid #dddddd;
    display: block;
    float: left;
    overflow: hidden;
    margin: 13px 0 0;
    padding: 7px;
    width: auto; }
  span.frame span img {
    display: block;
    float: left; }
  span.frame span span {
    clear: both;
    color: #333333;
    display: block;
    padding: 5px 0 0; }
span.align-center {
  display: block;
  overflow: hidden;
  clear: both; }
  span.align-center > span {
    display: block;
    overflow: hidden;
    margin: 13px auto 0;
    text-align: center; }
  span.align-center span img {
    margin: 0 auto;
    text-align: center; }
span.align-right {
  display: block;
  overflow: hidden;
  clear: both; }
  span.align-right > span {
    display: block;
    overflow: hidden;
    margin: 13px 0 0;
    text-align: right; }
  span.align-right span img {
    margin: 0;
    text-align: right; }
span.float-left {
  display: block;
  margin-right: 13px;
  overflow: hidden;
  float: left; }
  span.float-left span {
    margin: 13px 0 0; }
span.float-right {
  display: block;
  margin-left: 13px;
  overflow: hidden;
  float: right; }
  span.float-right > span {
    display: block;
    overflow: hidden;
    margin: 13px auto 0;
    text-align: right; }

code, tt {
  margin: 0 2px;
  padding: 0 5px;
  white-space: nowrap;
  border: 1px solid #eaeaea;
  background-color: #f8f8f8;
  border-radius: 3px; }

pre code {
  margin: 0;
  padding: 0;
  white-space: pre;
  border: none;
  background: transparent; }

.highlight pre {
  background-color: #f8f8f8;
  border: 1px solid #cccccc;
  font-size: 13px;
  line-height: 19px;
  overflow: auto;
  padding: 6px 10px;
  border-radius: 3px; }

pre {
  background-color: #f8f8f8;
  border: 1px solid #cccccc;
  font-size: 13px;
  line-height: 19px;
  overflow: auto;
  padding: 6px 10px;
  border-radius: 3px; }
  pre code, pre tt {
    background-color: transparent;
    border: none; }
