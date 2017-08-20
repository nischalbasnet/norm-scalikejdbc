package com.nischal.norm

import scalikejdbc.SQLSyntax

trait INormModel
{
  def insertSQL(exclude: Seq[String] = Seq.empty): Map[String, SQLSyntax]

  def updateSQL(): Map[String, SQLSyntax]
}

trait INormModelUpdater
{
  def updateSQL(nullables: Seq[String]): Map[String, SQLSyntax]
}