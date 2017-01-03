{
  try
  {
    ArrayList dbcolumns = new ArrayList<anonymColumn>();
    String sqlConnection = dbinfo.getUrl();
    Class.forName("com.mysql.jdbc.Driver").newInstance();
    connection = DriverManager.getConnection(sqlConnection, dbinfo.getUsername(), dbinfo.getPassword());
    PreparedStatement ps = null;
    String query = "SELECT table_name, column_name, data_type, column_key " + "FROM information_schema.columns " + "WHERE table_schema = \"" + dbinfo.getDbName() + "\"" + "AND (data_type = \"longtext\" OR data_type = \"varchar\");";
    ps = connection.prepareStatement(query);
    ResultSet s = ps.executeQuery();
    while (s.next())
    {
      String tableName = s.getString("table_name");
      String columnName = s.getString("column_name");
      String dataType = s.getString("data_type");
      String columnKey = s.getString("column_key");
      boolean isUnique = columnKey.compareToIgnoreCase("UNI") == 0 ? true : false;
      if (tableName.equalsIgnoreCase("hourentry") && columnName.equalsIgnoreCase("DTYPE") || tableName.equalsIgnoreCase("backlogs") && columnName.equalsIgnoreCase("backlogtype") || tableName.equalsIgnoreCase("backlogs_aud") && columnName.equalsIgnoreCase("backlogtype") || tableName.equalsIgnoreCase("widgets") && columnName.equalsIgnoreCase("type"))
      {
        continue;
      }
      anonymColumn col = new anonymColumn(tableName, columnName, dataType, isUnique);
      dbcolumns.add(col);
    }
    ps.close();
    connection.close();
    this.columns = dbcolumns;
  }
  catch (SQLException e)
  {
    System.out.println(("can not get columns from agilefant " + e.getCause()));
    System.out.println(("can not get columns from agilefant " + e.getMessage()));
  }
}