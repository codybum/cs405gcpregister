package edu.uky.cs405g;

import com.google.gson.Gson;
import edu.uky.cs405g.database.DBEngine;
import edu.uky.cs405g.httpfilters.AuthenticationFilter;
import org.glassfish.grizzly.http.server.HttpServer;
import org.glassfish.jersey.grizzly2.httpserver.GrizzlyHttpServerFactory;
import org.glassfish.jersey.server.ResourceConfig;

import javax.ws.rs.core.UriBuilder;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;


public class Launcher {

    public static Gson gson;
    public static DBEngine dbEngine;

    public static void main(String[] args) throws IOException {

        gson = new Gson();

        //Database Client initialization
        String DBuser = "";
        String DBpassword = "";
        String DBhost = "[your hostname here].netlab.uky.edu";
        String DBname = "";

        System.out.println("Starting Database...");
        dbEngine = new DBEngine(DBhost, DBname, DBuser, DBpassword);

        //Empty DB init
        //dbInit();

        //Database Table initialization
        //buildDataBase();

        //Embedded HTTP initialization
        startServer();

        try {
            while (true) {
                Thread.sleep(5000);
            }
        }catch (Exception ex) {
            ex.printStackTrace();
        }

    }


    private static void startServer() throws IOException {

        final ResourceConfig rc = new ResourceConfig()
                .packages("edu.uky.cs405g.httpcontrollers")
                .register(AuthenticationFilter.class);

        System.out.println("Starting Web Server...");
        URI BASE_URI = UriBuilder.fromUri("http://0.0.0.0/").port(9998).build();
        HttpServer httpServer = GrizzlyHttpServerFactory.createHttpServer(BASE_URI, rc);

        try {
            httpServer.start();

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static void dbInit() {

        /*
        #location
        create table location(lid int, address varchar(100) not null, primary key (lid))

        insert into location values (0,'800 Rose St. Lexington, Ky')
        */
    }

    private static void buildDataBase() {

        //location of class roster
        String locationOfClassList = "/Users/vcbumg2/Documents/Mesh/Work/CS/Classes/CS405G/cs405gUtils/accounts.csv";


        dbEngine.dropTable("ismember");
        dbEngine.dropTable("teams");
        dbEngine.dropTable("users");

        String createUsersTable = "create table users (id varchar(10) NOT NULL, primary key (id))";
        String createTeamsTable = "CREATE TABLE teams ( " +
                "     id VARCHAR(50) NOT NULL, " +
                "     name VARCHAR(30) NOT NULL UNIQUE, " +
                "     PRIMARY KEY (id) )";
        String createIsMemberTable = "create table ismember (" +
                "   team_id VARCHAR(50), " +
                "   user_id VARCHAR(10), " +
                "   foreign key (team_id) references teams(id), " +
                "   foreign key (user_id) references users(id))";

        //create users table
        dbEngine.executeUpdate(createUsersTable);

        //create teams table
        dbEngine.executeUpdate(createTeamsTable);

        //create isMemberTable
        dbEngine.executeUpdate(createIsMemberTable);


        //create a list for the roster
        List<String> userList = buildUserList(locationOfClassList);

        //insert class roster into user table
        for(String user : userList) {

            dbEngine.addUser(user);

            if(dbEngine.userExist(user)) {
                System.out.println("Added User : " + user);
            }

        }

    }

    public static List<String> buildUserList(String fileName) {
        List<String> userList = null;
        try {
            userList = new ArrayList<>();

            try(BufferedReader br = new BufferedReader(new FileReader(fileName))) {
                for(String line; (line = br.readLine()) != null; ) {
                    // process the line.
                    userList.add(line);
                }
                // line is not visible here.
            }

        } catch (Exception ex){
            ex.printStackTrace();
        }
        return  userList;

    }




}
