{
  String sqlConnection = dbinfo.getUrl();
  try
  {
    ArrayList dbtables = new ArrayList<String>();
    Class.forName("com.mysql.jdbc.Driver").newInstance();
    connection = DriverManager.getConnection(sqlConnection, dbinfo.getUsername(), dbinfo.getPassword());
    statement = connection.createStatement();
    PreparedStatement ps = null;
    String query = "select table_name from information_schema.tables WHERE table_schema = \"" + dbinfo.getDbName() + "\" and table_name LIKE \"anonym_%\";";
    ps = connection.prepareStatement(query);
    ResultSet s = ps.executeQuery();
    while (s.next())
    {
      dbtables.add(s.getString("table_name"));
    }
    int counter = 0;
    int tablesize = dbtables.size();
    statement = connection.createStatement();
    while (tablesize > counter)
    {
      statement.executeUpdate(("Drop table " + dbtables.get(counter)));
      counter++;
    }
    ps.close();
    connection.close();
    statement.close();
  }
  catch (SQLException e)
  {
    System.out.println(("Error Clean up anonymous tables agilefant " + e.getCause()));
    System.out.println(("Error Clean up anonymous tables agilefant " + e.getMessage()));
  }
}