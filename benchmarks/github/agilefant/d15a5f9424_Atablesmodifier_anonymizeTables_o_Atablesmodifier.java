{
  try
  {
    DbPropertiesReader properties = new DbPropertiesReader();
    String sqlConnection = "jdbc:mysql://" + properties.getDbHost() + ":3306" + "/" + properties.getDbName();
    Class.forName("com.mysql.jdbc.Driver").newInstance();
    connection = DriverManager.getConnection(sqlConnection, properties.getDbUsername(), properties.getDbPassword());
    statement = connection.createStatement();
    for (int i = 0 ; i < this.columns.size() ; i++)
    {
      String tableName = this.columns.get(i).tablename;
      String columnName = this.columns.get(i).columnname;
      if (this.columns.get(i).isUnique)
      {
        String query = "UPDATE anonym_" + tableName + " SET " + columnName + " = id;";
        statement.executeUpdate(query);
      }
      else
      {
        String query = "UPDATE anonym_" + tableName + " SET " + columnName + " = CONCAT(\"" + tableName + " \"," + "id, \" - length:\", LENGTH(" + columnName + "));";
        statement.executeUpdate(query);
      }
    }
    statement.close();
    connection.close();
  }
  catch (SQLException e)
  {
    System.out.println(("can not anonymize tables" + e.getCause()));
    System.out.println(("can not anonymize tables" + e.getMessage()));
  }
}