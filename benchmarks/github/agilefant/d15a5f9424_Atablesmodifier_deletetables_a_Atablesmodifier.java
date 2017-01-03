{
  String sqlConnection = dbinfo.getUrl();
  try
  {
    Class.forName("com.mysql.jdbc.Driver").newInstance();
    connection = DriverManager.getConnection(sqlConnection, dbinfo.getUsername(), dbinfo.getPassword());
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