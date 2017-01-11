package br.gov.lexml.parser.pl.output

import junit.framework.TestCase
import junit.framework.Assert.assertEquals

class LexmlRendererTest extends TestCase {

  def test_render_number_1_to_letter_a() {
    assertEquals(LexmlRenderer.renderAlphaSeq(1), "a")
  }

  def test_render_number_26_to_letter_z() {
    assertEquals(LexmlRenderer.renderAlphaSeq(26), "z")
  }

  def test_render_number_27_to_letter_aa() {
    assertEquals(LexmlRenderer.renderAlphaSeq(27), "aa")
  }

  def test_render_number_0_to_empty_string() {
    assertEquals(LexmlRenderer.renderAlphaSeq(0), "")
  }
}

