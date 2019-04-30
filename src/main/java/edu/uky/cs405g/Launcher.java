package edu.uky.cs405g;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import edu.uky.cs405g.database.DBEngine;
import edu.uky.cs405g.httpfilters.AuthenticationFilter;
import org.glassfish.grizzly.http.server.HttpServer;
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.client.ClientProperties;
import org.glassfish.jersey.grizzly2.httpserver.GrizzlyHttpServerFactory;
import org.glassfish.jersey.server.ResourceConfig;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.lang.reflect.Type;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class Launcher {

    public static Gson gson;
    public static DBEngine dbEngine;

    private static Type mapType;

    public static void main(String[] args) throws IOException {

        gson = new Gson();
        mapType = new TypeToken<Map<String, String>>() {
        }.getType();

        //Database Client initialization
        String DBuser = "";
        String DBpassword = "";
        String DBhost = "[your hostname here].netlab.uky.edu";
        String DBname = "";

        DBuser = "registration";
        DBpassword = "u$register01";
        DBhost = "vcbumg2.netlab.uky.edu";
        DBname = "registration";

        System.out.println("Starting Database...");
        dbEngine = new DBEngine(DBhost, DBname, DBuser, DBpassword);

        //checkTeamEndpoint();

        //gradeEndpoint("http://localhost:9990/api");
        String endpoint = "http://localhost:9990/api";

        checkEndpoint(endpoint);

        //System.out.println(checkStatus("http://localhost:9990/api"));
        //gradeEndpoint("http://bmwe238.netlab.uky.edu:9990/api");

        System.exit(0);

        //checkUserList();
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

    //dnbr225
    //smdamo2

    public static int checkEndpoint(String endpoint) {
        String dept_address = "800 Rose Street.";
        String department_id = "d-0";
        String service_id = "s-0";
        String taxid = "8808-080";

        String npi = "n-000-000";

        String pat_address = "165 Hill and Dale";
        String pid = "p-0-0-0";
        String ssn = "123-233-1333";

        String patient_data = "BP 120/70";
        String data_id = "d-0-0-0-0";

        int points = 0;
        points += checkStatus(endpoint);
        points += checkService("http://localhost:9990/api",dept_address,department_id,service_id,taxid);
        points += checkProvider("http://localhost:9990/api",npi);
        points += checkPatient(endpoint, pat_address, npi, pid, ssn);
        points += checkPatientData(endpoint, patient_data, pid, service_id, data_id);

        System.out.println("Total API Points: " + points);

        return points;
    }

    public static int checkStatus(String endpoint) {

        int points = 0;

        System.out.println("--Checking API Status");
        //Check Status
        String status = getURL(endpoint, "/status", 5000);

        if(status != null) {
            points += 10;
            System.out.println("\t\t+10 : API Status Check is Live");
            try {
                Map<String, String> myMap = gson.fromJson(status, mapType);

                int statusCode = Integer.parseInt(myMap.get("status_code"));
                if (statusCode == 1) {
                    points += 10;
                    System.out.println("\t\t+10 : API Status Check returned correct response");
                } else {
                    System.out.println("\t\t-10 : API Status Check returned incorrect response");
                }

            } catch (Exception exx) {
                System.out.println("\t\t-10 : API Status Check unable to parse response");
            }
        } else {
            System.out.println("\t\t-20 : API Status Check returned NULL");
        }
        System.out.println("\t->Checking API Status: Score: [" + points + "]\n");
        return points;
    }

    public static int checkService(String endpoint, String address, String department_id, String service_id, String taxid) {

        int points = 0;

        System.out.println("--Checking API Service");

        System.out.println("\t-Checking API /addservice");
        //Check Add Service


        Map<String,String> serviceMap = new HashMap();
        serviceMap.put("address",address);
        serviceMap.put("department_id",department_id);
        serviceMap.put("service_id",service_id);
        serviceMap.put("taxid",taxid);

        String servicePayload = gson.toJson(serviceMap);
        System.out.println("\t\t Service Payload [" + servicePayload + "]");

        String serviceAddReturn = postURL(servicePayload,endpoint,"addservice",5000);

        if(serviceAddReturn != null) {
            try {
                Map<String, String> myMap = gson.fromJson(serviceAddReturn, mapType);

                int statusCode = Integer.parseInt(myMap.get("status_code"));
                if (statusCode == 1) {
                    points += 10;
                    System.out.println("\t\t+10 : API /addservice returned correct response");
                } else {
                    points += 5;
                    System.out.println("\t\t+5 : API /addservice returned incorrect response");
                }
            } catch (Exception e) {
                points += 5;
                System.out.println("\t\t+5 : API /addservice unable to parse response");
            }
        } else {
            System.out.println("\t\t-10 : API /addservice return null");
        }

        System.out.println("\t-Checking API /getservice");

        //Check Get Service
        String serviceGetReturn = getURL(endpoint,"/getservice/" + service_id, 5000);

        if(serviceGetReturn != null) {


            Map<String, String> returnedServiceMap = gson.fromJson(serviceGetReturn, mapType);

            try {
                boolean isOk = true;

                for (Map.Entry<String, String> entry : serviceMap.entrySet()) {
                    String key = entry.getKey();
                    //String value = entry.getValue();
                    if (returnedServiceMap.containsKey(key)) {
                        if (!returnedServiceMap.get(key).equals(serviceMap.get(key))) {
                            isOk = false;
                            System.out.println("\t\t Value for [" + key + "] mismatch from /getservice return");
                        }
                    } else {
                        isOk = false;
                        System.out.println("\t\t Missing [" + key + "] from /getservice return");
                    }
                }

                if (isOk) {
                    points += 10;
                    System.out.println("\t\t+10 : API /getservice returned correct response");
                } else {
                    points += 5;
                    System.out.println("\t\t+5 : API /getservice return map did not match all values");
                }
            } catch(Exception ex) {
                points +=5;
                System.out.println("\t\t+5 : API /getservice return unable to parse");
            }

        } else {
            System.out.println("\t\t-10 : API /getservice return null");
        }
        System.out.println("\t->Checking Service: Score: [" + points + "]\n");
        //System.out.println("getService: " + serviceGetReturn);
        return points;
    }

    public static int checkProvider(String endpoint, String npi) {

        String postpath = "addprovider";
        String getpath = "getprovider";

        int points = 0;

        System.out.println("--Checking API Provider");

        System.out.println("\t-Checking API /addprovider");
        //Check Add Service


        Map<String,String> serviceMap = new HashMap();
        serviceMap.put("department_id","d-0");
        serviceMap.put("npi",npi);


        String servicePayload = gson.toJson(serviceMap);

        System.out.println("\t\t Provider Payload [" + servicePayload + "]");


        String serviceAddReturn = postURL(servicePayload,endpoint,postpath,5000);

        if(serviceAddReturn != null) {
            try {
                Map<String, String> myMap = gson.fromJson(serviceAddReturn, mapType);

                int statusCode = Integer.parseInt(myMap.get("status_code"));
                if (statusCode == 1) {
                    points += 5;
                    System.out.println("\t\t+5 : API /"+ postpath + " returned correct response");
                } else {
                    points += 2;
                    System.out.println("\t\t+2 : API /"+ postpath + " returned incorrect response");
                }
            } catch (Exception e) {
                points +=2;
                System.out.println("\t\t+2 : API /" + postpath + "unable to parse response");
            }
        } else {
            System.out.println("\t\t-5 : API /"+ postpath + " return null");
        }


        System.out.println("\t-Checking API /getprovider");

        //Check Get Service
        String serviceGetReturn = getURL(endpoint,getpath + "/" + npi, 5000);


        if(serviceGetReturn != null) {


            Map<String, String> returnedServiceMap = gson.fromJson(serviceGetReturn, mapType);

            try {
                boolean isOk = true;

                for (Map.Entry<String, String> entry : serviceMap.entrySet()) {
                    String key = entry.getKey();
                    //String value = entry.getValue();
                    if (returnedServiceMap.containsKey(key)) {
                        if (!returnedServiceMap.get(key).equals(serviceMap.get(key))) {
                            isOk = false;
                            System.out.println("\t\t Value for [" + key + "] mismatch from /" + getpath + " return");
                        }
                    } else {
                        isOk = false;
                        System.out.println("\t\t Missing [" + key + "] from /"+ getpath + " return");
                    }
                }

                if (isOk) {
                    points += 5;
                    System.out.println("\t\t+5 : API /"+ getpath + " returned correct response");
                } else {
                    points += 4;
                    System.out.println("\t\t+4 : API /"+ getpath + " return map did not match all values");
                }
            } catch(Exception ex) {
                points +=2;
                System.out.println("\t\t+3 : API /"+ getpath + " return unable to parse");
            }

        } else {
            System.out.println("\t\t-5 : API /"+ getpath + " return null");
        }


        System.out.println("\t->Checking Provider: Score: [" + points + "]\n");
        //System.out.println("getService: " + serviceGetReturn);

        return points;
    }

    public static int checkPatient(String endpoint, String address, String npi, String pid, String ssn) {


        String postpath = "addpatient";
        String getpath = "getpatient";

        int points = 0;

        System.out.println("--Checking API Patient");

        System.out.println("\t-Checking API /addpatient");
        //Check Add Service


        Map<String,String> serviceMap = new HashMap();
        serviceMap.put("address",address);
        serviceMap.put("provider_id",npi);
        serviceMap.put("pid", pid);
        serviceMap.put("ssn", ssn);


        String servicePayload = gson.toJson(serviceMap);

        System.out.println("\t\t Patient Payload [" + servicePayload + "]");


        String serviceAddReturn = postURL(servicePayload,endpoint,postpath,5000);

        if(serviceAddReturn != null) {
            try {
                Map<String, String> myMap = gson.fromJson(serviceAddReturn, mapType);

                int statusCode = Integer.parseInt(myMap.get("status_code"));
                if (statusCode == 1) {
                    points += 5;
                    System.out.println("\t\t+5 : API /"+ postpath + " returned correct response");
                } else {
                    points += 2;
                    System.out.println("\t\t+2 : API /"+ postpath + " returned incorrect response");
                }
            } catch (Exception e) {
                points +=2;
                System.out.println("\t\t+2 : API /" + postpath + "unable to parse response");
            }
        } else {
            System.out.println("\t\t-5 : API /"+ postpath + " return null");
        }


        System.out.println("\t-Checking API /getpatient");

        //Check Get Service
        String serviceGetReturn = getURL(endpoint,getpath + "/" + pid, 5000);


        if(serviceGetReturn != null) {


            Map<String, String> returnedServiceMap = gson.fromJson(serviceGetReturn, mapType);

            try {
                boolean isOk = true;

                for (Map.Entry<String, String> entry : serviceMap.entrySet()) {
                    String key = entry.getKey();
                    //String value = entry.getValue();
                    if (returnedServiceMap.containsKey(key)) {
                        if (!returnedServiceMap.get(key).equals(serviceMap.get(key))) {
                            isOk = false;
                            System.out.println("\t\t Value for [" + key + "] mismatch from /" + getpath + " return");
                        }
                    } else {
                        isOk = false;
                        System.out.println("\t\t Missing [" + key + "] from /"+ getpath + " return");
                    }
                }

                if (isOk) {
                    points += 5;
                    System.out.println("\t\t+5 : API /"+ getpath + " returned correct response");
                } else {
                    points += 4;
                    System.out.println("\t\t+4 : API /"+ getpath + " return map did not match all values");
                }
            } catch(Exception ex) {
                points +=2;
                System.out.println("\t\t+3 : API /"+ getpath + " return unable to parse");
            }

        } else {
            System.out.println("\t\t-5 : API /"+ getpath + " return null");
        }


        System.out.println("\t->Checking Patient: Score: [" + points + "]\n");
        //System.out.println("getService: " + serviceGetReturn);
        return points;
    }

    public static int checkPatientData(String endpoint, String data, String patient_id, String service_id, String id) {


        String postpath = "adddata";
        String getpath = "getdata";

        int points = 0;

        System.out.println("--Checking API Patient Data");

        System.out.println("\t-Checking API /adddata");
        //Check Add Service


        Map<String,String> serviceMap = new HashMap();
        serviceMap.put("data",data);
        serviceMap.put("patient_id",patient_id);
        serviceMap.put("service_id", service_id);
        serviceMap.put("id", id);


        String servicePayload = gson.toJson(serviceMap);

        System.out.println("\t\t Patient Data Payload [" + servicePayload + "]");


        String serviceAddReturn = postURL(servicePayload,endpoint,postpath,5000);

        if(serviceAddReturn != null) {
            try {
                Map<String, String> myMap = gson.fromJson(serviceAddReturn, mapType);

                int statusCode = Integer.parseInt(myMap.get("status_code"));
                if (statusCode == 1) {
                    points += 5;
                    System.out.println("\t\t+5 : API /"+ postpath + " returned correct response");
                } else {
                    points += 2;
                    System.out.println("\t\t+2 : API /"+ postpath + " returned incorrect response");
                }
            } catch (Exception e) {
                points +=2;
                System.out.println("\t\t+2 : API /" + postpath + "unable to parse response");
            }
        } else {
            System.out.println("\t\t-5 : API /"+ postpath + " return null");
        }


        System.out.println("\t-Checking API /getdata");

        //Check Get Service
        String serviceGetReturn = getURL(endpoint,getpath + "/" + id, 5000);


        if(serviceGetReturn != null) {


            Map<String, String> returnedServiceMap = gson.fromJson(serviceGetReturn, mapType);

            try {
                boolean isOk = true;

                for (Map.Entry<String, String> entry : serviceMap.entrySet()) {
                    String key = entry.getKey();
                    //String value = entry.getValue();
                    if (returnedServiceMap.containsKey(key)) {
                        if (!returnedServiceMap.get(key).equals(serviceMap.get(key))) {
                            isOk = false;
                            System.out.println("\t\t Value for [" + key + "] mismatch from /" + getpath + " return");
                        }
                    } else {
                        isOk = false;
                        System.out.println("\t\t Missing [" + key + "] from /"+ getpath + " return");
                    }
                }

                if (isOk) {
                    points += 5;
                    System.out.println("\t\t+5 : API /"+ getpath + " returned correct response");
                } else {
                    points += 4;
                    System.out.println("\t\t+4 : API /"+ getpath + " return map did not match all values");
                }
            } catch(Exception ex) {
                points +=2;
                System.out.println("\t\t+3 : API /"+ getpath + " return unable to parse");
            }

        } else {
            System.out.println("\t\t-5 : API /"+ getpath + " return null");
        }


        System.out.println("\t->Checking Patient Data: Score: [" + points + "]\n");
        //System.out.println("getService: " + serviceGetReturn);
        return points;
    }


    public static String getURL(String endpoint, String path, int timeout) {

        ClientConfig configuration = new ClientConfig();
        configuration.property(ClientProperties.CONNECT_TIMEOUT, timeout);
        configuration.property(ClientProperties.READ_TIMEOUT, timeout);
        Client client = ClientBuilder.newClient(configuration);

        String status = null;

        try {
            status = client
                    .target(endpoint)
                    .path(path)
                    .request(MediaType.APPLICATION_JSON)
                    .get(String.class);
        } catch (javax.ws.rs.NotFoundException uex) {
            System.out.println("status_warning endpoint " + endpoint + " does not exist!");
        } catch (javax.ws.rs.ProcessingException cex){
            System.out.println("status_warning endpoint " + endpoint + " Connection refused !");
        }

        return status;
    }

    public static String postURL(String payload, String endpoint, String path, int timeout) {

        ClientConfig configuration = new ClientConfig();
        configuration.property(ClientProperties.CONNECT_TIMEOUT, timeout);
        configuration.property(ClientProperties.READ_TIMEOUT, timeout);
        Client client = ClientBuilder.newClient(configuration);

        String status = null;

        try {

            Response result = client
                    .target(endpoint)
                    .path(path)
                    .request(MediaType.APPLICATION_JSON)
                    .post(Entity.entity(payload, MediaType.APPLICATION_JSON));

            status = result.readEntity(String.class);

            /*
            status = client
                    .target(endpoint)
                    .path(path)
                    .request(MediaType.APPLICATION_JSON)
                    .get(String.class);
            */
        } catch (javax.ws.rs.NotFoundException uex) {
            System.out.println("status_warning endpoint " + endpoint + " does not exist!");
        } catch (javax.ws.rs.ProcessingException cex){
            System.out.println("status_warning endpoint " + endpoint + " Connection refused !");
        }

        return status;
    }


    private static void checkTeamEndpoint() {

        Map<String,String> endpointMap = null;
        Map<String,String> teamMap = null;
        try {


            endpointMap = dbEngine.getEndpointMap();
            teamMap = dbEngine.getTeamIdMap();


            Type mapType = new TypeToken<Map<String, String>>() {
            }.getType();

            for (Map.Entry<String, String> entry : endpointMap.entrySet()) {
                String team_id = entry.getKey();
                String value = entry.getValue();

                //System.out.println(key + "-" + value);

                ClientConfig configuration = new ClientConfig();
                configuration.property(ClientProperties.CONNECT_TIMEOUT, 5000);
                configuration.property(ClientProperties.READ_TIMEOUT, 5000);
                Client client = ClientBuilder.newClient(configuration);


                //Client client = ClientBuilder.newClient();

                String status = null;

                try {
                    status = client
                            .target(value + "/status")
                            .request(MediaType.APPLICATION_JSON)
                            .get(String.class);
                } catch (javax.ws.rs.NotFoundException uex) {
                    //System.out.println("status_warning","endpoint " + endpoint + " does not exist!");
                } catch (javax.ws.rs.ProcessingException cex){
                    //responseMap.put("status_warning","endpoint " + endpoint + " Connection refused !");
                }

                List<String> teamList = dbEngine.getTeamList(team_id);

                //System.out.println("Team: " + teamMap.get(team_id) + " member count:" + teamList.size());


                if(status != null) {

                    try {
                        Map<String, String> myMap = gson.fromJson(status, mapType);

                        int statusCode = Integer.parseInt(myMap.get("status_code"));
                        if(statusCode == 1) {
                            //responseMap.put("success", Boolean.TRUE.toString());
                            //responseMap.put("status_desc","status_code " + statusCode);

                            //System.out.print(teamMap.get(team_id) + ", ");

                        }

                    } catch (Exception exx) {
                        //responseMap.put("success", Boolean.FALSE.toString());
                        //responseMap.put("status_desc","unable to parse status return " + status);
                    }


                } else {

                    if(teamList.size() > 0) {
                        System.out.println("--");
                        System.out.println("CS405G : Final Project API NOT RESPONDING");
                        System.out.println("Team: [" + teamMap.get(team_id) + "]");
                        System.out.println("\nThe following error is preventing the grading of your final project:");
                        System.out.println("Error: No status returned from : " + endpointMap.get(team_id));
                        System.out.println("\nYou will have until midnight Wednesday, 5/1 to fix this issue before grades are submitted.");
                        System.out.println("\n-Cody");

                        for(String user_id : teamList) {
                            System.out.print(user_id + "@uky.edu; ");
                        }
                        System.out.println("");
                    }
                    //responseMap.put("success", Boolean.FALSE.toString());
                    //responseMap.put("status_desc","No data returned for endpoint " + endpoint);
                }

            }

        }catch (Exception e) {
            e.printStackTrace();
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

    private static void checkUserList() {
        String locationOfClassList = "/Users/cody/Downloads/accounts.csv";

        //create a list for the roster
        List<String> userList = buildUserList(locationOfClassList);

        //insert class roster into user table
        for(String user : userList) {

            if(!dbEngine.userOnTeam(user)) {
                System.out.print(user + "@uky.edu;");
                //System.out.println(user);
            }

        }


    }

    /*
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

*/
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
