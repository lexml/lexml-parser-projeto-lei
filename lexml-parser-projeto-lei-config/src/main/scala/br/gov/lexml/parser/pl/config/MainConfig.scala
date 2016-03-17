package br.gov.lexml.parser.pl.config

import com.typesafe.config.ConfigFactory

object MainConfig {
  lazy val config = ConfigFactory.load()
}