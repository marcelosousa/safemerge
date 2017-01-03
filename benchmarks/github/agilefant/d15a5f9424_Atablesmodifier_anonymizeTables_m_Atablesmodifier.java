{
  String err_table = "";
  String err_column = "";
  try
  {
    String sqlConnection = dbinfo.getUrl();
    Class.forName("com.mysql.jdbc.Driver").newInstance();
    connection = DriverManager.getConnection(sqlConnection, dbinfo.getUsername(), dbinfo.getPassword());
    statement = connection.createStatement();
    for (int i = 0 ; i < this.columns.size() ; i++)
    {
      String tableName = this.columns.get(i).tablename;
      String columnName = this.columns.get(i).columnname;
      err_table = this.columns.get(i).tablename;
      err_column = this.columns.get(i).columnname;
      if (this.columns.get(i).isUnique)
      {
        String query = "UPDATE anonym_" + tableName + " SET " + columnName + " = id;";
        statement.executeUpdate(query);
      }
      else
      {
        if ((tableName.compareToIgnoreCase("users") == 0 && columnName.compareToIgnoreCase("password")) == 0)
        {
          String pw = SecurityUtil.MD5("password");
          String query = "UPDATE anonym_" + tableName + " SET " + columnName + " = \"" + pw + "\";";
          statement.executeUpdate(query);
        }
        else
        {
          String query = "UPDATE anonym_" + tableName + " SET " + columnName + " = CONCAT(\"" + tableName + " \"," + "id, \" - length:\", LENGTH(" + columnName + "),\" - hash:\",MD5(" + columnName + "));";
          statement.executeUpdate(query);
        }
      }
    }
    statement.close();
    connection.close();
  }
  catch (SQLException e)
  {
    System.out.println(("can not anonymize table: " + err_table + " Column: " + err_column + "cuz " + e.getCause()));
    System.out.println(("can not anonymize table: " + err_table + " Column: " + err_column + "cuz " + e.getMessage()));
  }
}