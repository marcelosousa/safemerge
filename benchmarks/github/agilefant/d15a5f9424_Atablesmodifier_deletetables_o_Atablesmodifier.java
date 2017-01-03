{
  DbPropertiesReader properties = new DbPropertiesReader();
  String sqlConnection = "jdbc:mysql://" + properties.getDbHost() + ":3306" + "/" + properties.getDbName();
  try
  {
    Class.forName("com.mysql.jdbc.Driver").newInstance();
    connection = DriverManager.getConnection(sqlConnection, properties.getDbUsername(), properties.getDbPassword());
    statement = connection.createStatement();
    int counter = 0;
    int tablesize = tables.size();
    while (tablesize > counter)
    {
      statement.executeUpdate(("Drop table anonym_" + tables.get(counter)));
      counter++;
    }
    statement.close();
    connection.close();
  }
  catch (SQLException e)
  {
    System.out.println(("delete " + e.getCause()));
    System.out.println(("delete " + e.getMessage()));
  }
}