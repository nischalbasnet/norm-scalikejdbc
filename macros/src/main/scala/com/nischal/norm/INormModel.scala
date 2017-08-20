package com.nischal.norm

trait INormModel
{
  def insertSQL(exclude: Seq[String] = Seq.empty): Map[String, scalikejdbc.SQLSyntax]

  def updateSQL(): Map[String, scalikejdbc.SQLSyntax]
}

trait INormModelUpdater
{
  def updateSQL(nullables: Seq[String]): Map[String, scalikejdbc.SQLSyntax]
}