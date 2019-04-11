package edu.uky.cs405g.database;

import org.apache.commons.dbcp2.*;
import org.apache.commons.pool2.ObjectPool;
import org.apache.commons.pool2.impl.GenericObjectPool;

import javax.sql.DataSource;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.sql.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;



public class DBEngine {

    private DataSource ds;

    public boolean isInit = false;

    public DBEngine(String host, String database, String login, String password) {

        try {
            Class.forName("com.mysql.cj.jdbc.Driver").newInstance();

            String dbConnectionString = null;


            if(database == null) {

                dbConnectionString ="jdbc:mysql://" + host + "?" +
                        "user=" + login  +"&password=" + password + "&useUnicode=true&useJDBCCompliantTimezoneShift=true&useLegacyDatetimeCode=false&serverTimezone=UTC";
            } else {

                dbConnectionString ="jdbc:mysql://" + host + "/" + database  + "?" +
                        "user=" + login  +"&password=" + password + "&useUnicode=true&useJDBCCompliantTimezoneShift=true&useLegacyDatetimeCode=false&serverTimezone=UTC";
            }

            ds = setupDataSource(dbConnectionString);


            isInit = true;


        }

        catch (Exception ex) {
            ex.printStackTrace();
        }

    }

    public static DataSource setupDataSource(String connectURI) {
        //
        // First, we'll create a ConnectionFactory that the
        // pool will use to create Connections.
        // We'll use the DriverManagerConnectionFactory,
        // using the connect string passed in the command line
        // arguments.
        //
        ConnectionFactory connectionFactory = null;
            connectionFactory = new DriverManagerConnectionFactory(connectURI, null);


        //
        // Next we'll create the PoolableConnectionFactory, which wraps
        // the "real" Connections created by the ConnectionFactory with
        // the classes that implement the pooling functionality.
        //
        PoolableConnectionFactory poolableConnectionFactory =
                new PoolableConnectionFactory(connectionFactory, null);

        //
        // Now we'll need a ObjectPool that serves as the
        // actual pool of connections.
        //
        // We'll use a GenericObjectPool instance, although
        // any ObjectPool implementation will suffice.
        //
        ObjectPool<PoolableConnection> connectionPool =
                new GenericObjectPool<>(poolableConnectionFactory);

        // Set the factory's pool property to the owning pool
        poolableConnectionFactory.setPool(connectionPool);

        //
        // Finally, we create the PoolingDriver itself,
        // passing in the object pool we created.
        //
        PoolingDataSource<PoolableConnection> dataSource =
                new PoolingDataSource<>(connectionPool);

        return dataSource;
    }

    public boolean userExist(String linkblueId) {
        boolean exist = false;
        try {

            Connection conn = ds.getConnection();

            try {
                String queryString = null;


                //region
                queryString = "SELECT COUNT(1) " + "FROM users " +
                        "WHERE id = '" + linkblueId + "'";


                Statement stmt = conn.createStatement();


                ResultSet rs = stmt.executeQuery(queryString);

                if (rs.next()) {
                    exist = rs.getBoolean(1);
                }
                rs.close();
                stmt.close();
            } catch (Exception e) {
            e.printStackTrace();
        } finally {
            conn.close();
        }


        } catch(Exception ex) {
            ex.printStackTrace();
        }
        return exist;
    }

    public boolean teamExist(String teamId) {
        boolean exist = false;
        try {
            Connection conn = ds.getConnection();

            try {
                String queryString = null;


                //region
                queryString = "SELECT COUNT(1) " + "FROM teams " +
                        "WHERE id = '" + teamId + "'";


                Statement stmt = conn.createStatement();


                ResultSet rs = stmt.executeQuery(queryString);

                if (rs.next()) {
                    exist = rs.getBoolean(1);
                }
                rs.close();
                stmt.close();
            } catch (Exception e) {

                    e.printStackTrace();
                } finally {
                    conn.close();
                }

        } catch(Exception ex) {
            ex.printStackTrace();
        }
        return exist;
    }

    public boolean endpointExist(String teamId) {
        boolean exist = false;
        try {
            Connection conn = ds.getConnection();

            try {
                String queryString = null;


                //region
                queryString = "SELECT COUNT(1) " + "FROM endpoints " +
                        "WHERE team_id = '" + teamId + "'";


                Statement stmt = conn.createStatement();


                ResultSet rs = stmt.executeQuery(queryString);

                if (rs.next()) {
                    exist = rs.getBoolean(1);
                }
                rs.close();
                stmt.close();
            } catch (Exception e) {

                e.printStackTrace();
            } finally {
                conn.close();
            }

        } catch(Exception ex) {
            ex.printStackTrace();
        }
        return exist;
    }


    public boolean userOnTeam(String linkblueId) {
        boolean exist = false;
        try {

            Connection conn = ds.getConnection();
            try {
                String queryString = null;


                //region
                queryString = "SELECT COUNT(1) " + "FROM ismember " +
                        "WHERE user_id = '" + linkblueId + "'";


                Statement stmt = conn.createStatement();


                ResultSet rs = stmt.executeQuery(queryString);

                if (rs.next()) {
                    exist = rs.getBoolean(1);
                }
                rs.close();
                stmt.close();
            } catch (Exception e) {

                e.printStackTrace();
            } finally {
                conn.close();
            }


        } catch(Exception ex) {
            ex.printStackTrace();
        }
        return exist;
    }

    public int addUser(String linkblueId) {
        int result = -1;
        try {
            Connection conn = ds.getConnection();
            try {
                Statement stmt = conn.createStatement();
                String insertUserString = "INSERT INTO users (id) VALUES('" + linkblueId + "')";
                result = stmt.executeUpdate(insertUserString);
                stmt.close();
            } catch (Exception e) {

                e.printStackTrace();
            } finally {
                conn.close();
            }

        } catch(Exception ex) {
            ex.printStackTrace();
        }
        return  result;
    }

    public int addTeam(String teamId, String teamName) {
        int result = -1;
        try {
            Connection conn = ds.getConnection();
            try {
                Statement stmt = conn.createStatement();
                String insertUserString = "INSERT INTO teams VALUES('" + teamId + "','" + teamName + "')";
                //INSERT INTO teams (name) VALUES('blah')
                result = stmt.executeUpdate(insertUserString);
                stmt.close();

            } catch(SQLIntegrityConstraintViolationException exs) {
                result = -2;
            } catch (Exception e) {

                e.printStackTrace();
            } finally {
                conn.close();
            }

        } catch (Exception ex) {
            ex.printStackTrace();
        }
        return  result;
    }

    public int addUserToTeam(String teamId, String linkblueId) {
        int result = -1;
        try {
            Connection conn = ds.getConnection();
            try {
                Statement stmt = conn.createStatement();
                String insertUserString = "INSERT INTO ismember VALUES('" + teamId + "','" + linkblueId + "')";
                //INSERT INTO teams (name) VALUES('blah')
                result = stmt.executeUpdate(insertUserString);
                stmt.close();
            } catch (Exception e) {

                e.printStackTrace();
            } finally {
                conn.close();
            }


        } catch(Exception ex) {
            ex.printStackTrace();
        }
        return  result;
    }

    public int addEndpointToTeam(String endpointId, String teamId, String endpoint) {
        int result = -1;
        try {
            Connection conn = ds.getConnection();
            try {
                Statement stmt = conn.createStatement();
                String insertUserString = "INSERT INTO endpoints VALUES('" + endpointId + "','" + teamId + "','" + endpoint + "')";
                //INSERT INTO teams (name) VALUES('blah')
                result = stmt.executeUpdate(insertUserString);
                stmt.close();
            } catch (Exception e) {

                e.printStackTrace();
            } finally {
                conn.close();
            }


        } catch(Exception ex) {
            ex.printStackTrace();
        }
        return  result;
    }

    public int updateEndpointForTeam(String teamId, String endpoint) {
        int result = -1;
        try {
            Connection conn = ds.getConnection();
            try {
                Statement stmt = conn.createStatement();
                String insertUserString = "UPDATE endpoints SET endpoint='" + endpoint + "' WHERE team_id = '" + teamId +"'";
                result = stmt.executeUpdate(insertUserString);
                stmt.close();
            } catch (Exception e) {

                e.printStackTrace();
            } finally {
                conn.close();
            }


        } catch(Exception ex) {
            ex.printStackTrace();
        }
        return  result;
    }

    public String getTeamId(String teamName) {
        String configParams = null;
        try {

            Connection conn = ds.getConnection();
            try {
                String queryString = null;

                //region
                queryString = " SELECT id FROM teams " +
                        "WHERE name = '" + teamName + "'";


                Statement stmt = conn.createStatement();

                ResultSet rs = stmt.executeQuery(queryString);

                if (rs.next()) {
                    configParams = rs.getString(1);
                }

                rs.close();
                stmt.close();
            } catch (Exception e) {

                e.printStackTrace();
            } finally {
                conn.close();
            }

        } catch(Exception ex) {
            ex.printStackTrace();
            System.out.println(ex.getMessage());
            StringWriter errors = new StringWriter();
            ex.printStackTrace(new PrintWriter(errors));
            System.out.println(errors.toString());
        }
        return configParams;
    }

    public String getEndPoint(String teamId) {
        String configParams = null;
        try {

            Connection conn = ds.getConnection();
            try {
                String queryString = null;

                //region
                queryString = "SELECT endpoint FROM endpoints where team_id = '" + teamId + "'";


                Statement stmt = conn.createStatement();

                ResultSet rs = stmt.executeQuery(queryString);

                if (rs.next()) {
                    configParams = rs.getString(1);
                }

                rs.close();
                stmt.close();
            } catch (Exception e) {

                e.printStackTrace();
            } finally {
                conn.close();
            }

        } catch(Exception ex) {
            ex.printStackTrace();
            System.out.println(ex.getMessage());
            StringWriter errors = new StringWriter();
            ex.printStackTrace(new PrintWriter(errors));
            System.out.println(errors.toString());
        }
        return configParams;
    }

    public String getMyTeamId(String linkblueId) {
        String configParams = null;
        try {
            Connection conn = ds.getConnection();

            try {
                String queryString = null;

                //region
                queryString = " SELECT team_id FROM ismember " +
                        "WHERE user_id = '" + linkblueId + "'";


                //System.out.println("QUERY: [" + queryString + "]");


                Statement stmt = conn.createStatement();

                ResultSet rs = stmt.executeQuery(queryString);

                if (rs.next()) {
                    configParams = rs.getString(1);
                }
                rs.close();
                stmt.close();
            } catch (Exception e) {

                e.printStackTrace();
            } finally {
                conn.close();
            }

        } catch(Exception ex) {
            ex.printStackTrace();
            System.out.println(ex.getMessage());
            StringWriter errors = new StringWriter();
            ex.printStackTrace(new PrintWriter(errors));
            System.out.println(errors.toString());
        }
        return configParams;
    }

    public Map<String,String> getTeamIdMap() {
        Map<String,String> teamIdMap = new HashMap<>();

        Statement stmt = null;
        try
        {
            Connection conn = ds.getConnection();
            try {
                String queryString = null;

                queryString = "SELECT * FROM teams";

                stmt = conn.createStatement();

                ResultSet rs = stmt.executeQuery(queryString);

                while (rs.next()) {
                    String teamId = rs.getString("id");
                    String teamName = rs.getString("name");
                    teamIdMap.put(teamId, teamName);
                }

                rs.close();
                stmt.close();
            } catch (Exception e) {

                e.printStackTrace();
            } finally {
                conn.close();
            }

        }
        catch(Exception ex)
        {
            ex.printStackTrace();
        }

        return teamIdMap;
    }

    public int teamSize(String teamId) {
        int teamSize = -1;
        try {

            Connection conn = ds.getConnection();
            try {
                String queryString = null;


                queryString = "SELECT count(M.team_id) FROM teams T, ismember M " +
                        "WHERE T.id = M.team_id " +
                        "AND T.id = '" + teamId + "'";

                Statement stmt = conn.createStatement();


                ResultSet rs = stmt.executeQuery(queryString);

                if (rs.next()) {
                    teamSize = rs.getInt(1);
                }
                rs.close();
                stmt.close();
            } catch (Exception e) {

                e.printStackTrace();
            } finally {
                conn.close();
            }

        } catch(Exception ex) {
            ex.printStackTrace();
        }
        return teamSize;
    }

    public int removeUserFromTeam(String teamId, String userId) {
        int result = -1;
        try {
            Connection conn = ds.getConnection();
            try {
                Statement stmt = conn.createStatement();
                result = stmt.executeUpdate("DELETE FROM ismember WHERE user_id = '" + userId + "' and team_id = '" + teamId + "'");
                stmt.close();
            } catch (Exception e) {

                e.printStackTrace();
            } finally {
                conn.close();
            }

        } catch(Exception ex) {
            ex.printStackTrace();
        }
        return  result;
    }


    public int executeUpdate(String stmtString) {
        int result = -1;
        try {
            Connection conn = ds.getConnection();
            try {
                Statement stmt = conn.createStatement();
                result = stmt.executeUpdate(stmtString);
                stmt.close();
            } catch (Exception e) {

                e.printStackTrace();
            } finally {
                conn.close();
            }

        } catch(Exception ex) {
            ex.printStackTrace();
        }
        return  result;
    }

    public int dropTable(String tableName) {
        int result = -1;
        try {
            Connection conn = ds.getConnection();
            try {
                String stmtString = null;

                stmtString = "DROP TABLE " + tableName;

                Statement stmt = conn.createStatement();

                result = stmt.executeUpdate(stmtString);

                stmt.close();
            } catch (Exception e) {

                e.printStackTrace();
            } finally {
                conn.close();
            }

        } catch(Exception ex) {
            ex.printStackTrace();
        }
        return result;
    }

    public boolean databaseExist(String databaseName)  {
        boolean exist = false;

        Statement stmt = null;
        ResultSet rs = null;

        try {

            Connection conn = ds.getConnection();
            String queryString = null;

            queryString = "SELECT COUNT(1) FROM INFORMATION_SCHEMA.SCHEMATA " +
                    "WHERE SCHEMA_NAME  = N'" + databaseName + "'";

            stmt = conn.createStatement();

            rs = stmt.executeQuery(queryString);
            rs.next();
            exist = rs.getBoolean(1);

            rs.close();
            stmt.close();
            conn.close();

            //todo likely better way to do this hack to let derby work
        }
        catch(Exception ex) {
            ex.printStackTrace();
        }
        return exist;
    }

    public List<String> getDatabaseNames()  {

        List<String> databaseNames = null;
        Statement stmt = null;
        ResultSet rs = null;

        try {
            Connection conn = ds.getConnection();
            databaseNames = new ArrayList<>();

            String queryString = null;

            queryString = "SELECT SCHEMA_NAME FROM INFORMATION_SCHEMA.SCHEMATA";

            stmt = conn.createStatement();

            rs = stmt.executeQuery(queryString);

            while (rs.next()) {
                databaseNames.add(rs.getString(1));
            }
            rs.close();
            stmt.close();
            conn.close();

        }
        catch(Exception ex) {
            ex.printStackTrace();
        }

        return databaseNames;

    }

    public List<String> getTableNames(String database)  {

        List<String> tableNames = null;

        Statement stmt = null;
        ResultSet rs = null;

        try {

            Connection conn = ds.getConnection();
            tableNames = new ArrayList<>();
            String queryString = null;

            queryString = "SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_SCHEMA = '" + database + "'";

            stmt = conn.createStatement();

            rs = stmt.executeQuery(queryString);

            while (rs.next()) {
                tableNames.add(rs.getString(1));
            }
            rs.close();
            stmt.close();
            conn.close();
        }
        catch(Exception ex) {
            ex.printStackTrace();
        }
        return tableNames;
    }

    public List<String> getColumnNames(String database, String table)  {

        List<String> columnNames = null;

        Statement stmt = null;
        ResultSet rs = null;

        try {
            Connection conn = ds.getConnection();
            columnNames = new ArrayList<>();
            String queryString = null;

            queryString = "SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_SCHEMA='" + database + "' AND TABLE_NAME='" + table + "'";

            stmt = conn.createStatement();

            rs = stmt.executeQuery(queryString);

            while (rs.next()) {
                columnNames.add(rs.getString(1));
            }
            rs.close();
            stmt.close();
            conn.close();

        }
        catch(Exception ex) {
            ex.printStackTrace();
        }
        return columnNames;
    }

    public boolean tableExist(String tableName)  {
        boolean exist = false;

        Statement stmt = null;
        ResultSet rs = null;

        try {
            Connection conn = ds.getConnection();
            String queryString = null;

            queryString = "SELECT COUNT(1) FROM INFORMATION_SCHEMA.TABLES " +
                    "WHERE TABLE_NAME = N'" + tableName + "'";

            stmt = conn.createStatement();

            rs = stmt.executeQuery(queryString);
            rs.next();
            exist = rs.getBoolean(1);

            rs.close();
            stmt.close();
            conn.close();


            //todo likely better way to do this hack to let derby work
        }
        catch(Exception ex) {
            ex.printStackTrace();
        }
        return exist;
    }

    public void printDB() {
        try {

            Connection conn = ds.getConnection();
            Statement stmt = conn.createStatement();

            //System.out.println("NumActive: " + ds.getNumActive());
            //System.out.println("NumIdle: " + ds.getNumIdle());

            //Statement stmt = conn.createStatement();
            ResultSet rs = stmt.executeQuery("SELECT * FROM mysql.user");

            // print out query result
            while (rs.next()) {
                //System.out.printf("%d\t%s\n", rs.getInt("id"), rs.getString("name"));
                //System.out.print(".");
                String user = rs.getString("user");
                String host = rs.getString("host");

                System.out.println(user + ":" + host);
            }
            rs.close();
            stmt.close();
            conn.close();

        } catch(Exception ex) {
            ex.printStackTrace();
        }
    }



}
