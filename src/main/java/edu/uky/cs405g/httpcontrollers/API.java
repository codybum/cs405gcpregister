package edu.uky.cs405g.httpcontrollers;

import edu.uky.cs405g.Launcher;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.*;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@Path("/api")
public class API {


    @GET
    @Path("/listteams")
    @Produces(MediaType.APPLICATION_JSON)
    public Response listTeams(@HeaderParam("X-Auth-API-Key") String authKey) {
        String responseString = "{}";
        try {
            Map<String,String> teamMap = Launcher.dbEngine.getTeamIdMap();

            responseString = Launcher.gson.toJson(teamMap);
            //if (limitInt > 1000) {
                //return Response.status(500).entity("Currently the hard limit is 1000, please enter a value equal to or less than 1000.").build();
            //}
        } catch (Exception ex) {

            StringWriter sw = new StringWriter();
            ex.printStackTrace(new PrintWriter(sw));
            String exceptionAsString = sw.toString();
            ex.printStackTrace();

            return Response.status(500).entity(exceptionAsString).build();
        }
        return Response.ok(responseString).header("Access-Control-Allow-Origin", "*").build();
    }

    @GET
    @Path("/addteam/{teamname}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response addTeam(@HeaderParam("X-Auth-API-Key") String authKey, @PathParam("teamname") String teamName) {
        String responseString = "{}";
        try {

            Map<String,String> responseMap = new HashMap<>();

            String teamId = UUID.randomUUID().toString();
            responseMap.put("team_id",teamId);
            responseMap.put("team_name",teamName);

            String originalTeamId = Launcher.dbEngine.getTeamId(teamName);

            if(originalTeamId != null) {

                responseMap.put("success", Boolean.FALSE.toString());
                responseMap.put("status_desc", "team_name: '" + teamName + "' already exist as team_id: '" + originalTeamId + "'");

            } else {

                int queryStatus = Launcher.dbEngine.addTeam(teamId, teamName);
                if (queryStatus == 1) {
                    responseMap.put("success", Boolean.TRUE.toString());
                    responseMap.put("success_desc", "team added");
                } else if (queryStatus == -2) {
                    responseMap.put("success", Boolean.FALSE.toString());
                    responseMap.put("status_desc", "Constraint Violation Exception");
                } else if (queryStatus == -1) {
                    responseMap.put("success", Boolean.FALSE.toString());
                    responseMap.put("status_desc", "Unknown SQL Error");
                }
            }

            responseString = Launcher.gson.toJson(responseMap);

        } catch (Exception ex) {

            StringWriter sw = new StringWriter();
            ex.printStackTrace(new PrintWriter(sw));
            String exceptionAsString = sw.toString();
            ex.printStackTrace();

            return Response.status(500).entity(exceptionAsString).build();
        }
        return Response.ok(responseString).header("Access-Control-Allow-Origin", "*").build();
    }

    @GET
    @Path("/jointeam/{teamid}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response joinTeam(@HeaderParam("X-Auth-API-Key") String authKey, @PathParam("teamid") String teamId) {
        String responseString = "{}";
        try {

            Map<String,String> responseMap = new HashMap<>();

            responseMap.put("team_id",teamId);
            responseMap.put("user_id",authKey);

            if(Launcher.dbEngine.userExist(authKey)) {

                if(Launcher.dbEngine.teamExist(teamId)) {

                    if(!Launcher.dbEngine.userOnTeam(authKey)) {

                        if(Launcher.dbEngine.teamSize(teamId) < 4) {

                            int queryStatus = Launcher.dbEngine.addUserToTeam(teamId,authKey);

                            if(queryStatus == 1) {
                                responseMap.put("success", Boolean.TRUE.toString());
                                responseMap.put("success_desc", "added to team");
                            } else if(queryStatus == -2) {
                                responseMap.put("success", Boolean.FALSE.toString());
                                responseMap.put("status_desc","Constraint Violation Exception");
                            } else if(queryStatus == -1) {
                                responseMap.put("success", Boolean.FALSE.toString());
                                responseMap.put("status_desc","Unknown SQL Error");
                            }

                        } else {
                            responseMap.put("success", Boolean.FALSE.toString());
                            responseMap.put("status_desc","Team Limit MAX(4) : Too many people on the team team!");
                        }

                    } else {
                        responseMap.put("success", Boolean.FALSE.toString());
                        responseMap.put("status_desc","User already assigned to a team!");
                    }

                } else {
                    responseMap.put("success", Boolean.FALSE.toString());
                    responseMap.put("status_desc","Team does not exist!");
                }

            } else {
                responseMap.put("success", Boolean.FALSE.toString());
                responseMap.put("status_desc","User does not exist!");
            }


            responseString = Launcher.gson.toJson(responseMap);

        } catch (Exception ex) {

            StringWriter sw = new StringWriter();
            ex.printStackTrace(new PrintWriter(sw));
            String exceptionAsString = sw.toString();
            ex.printStackTrace();

            return Response.status(500).entity(exceptionAsString).build();
        }
        return Response.ok(responseString).header("Access-Control-Allow-Origin", "*").build();
    }

    @GET
    @Path("/unjointeam/{teamid}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response unJoinTeam(@HeaderParam("X-Auth-API-Key") String authKey, @PathParam("teamid") String teamId) {
        String responseString = "{}";
        try {

            Map<String,String> responseMap = new HashMap<>();

            responseMap.put("team_id",teamId);
            responseMap.put("user_id",authKey);

            if(Launcher.dbEngine.userExist(authKey)) {

                if(Launcher.dbEngine.teamExist(teamId)) {

                    if(Launcher.dbEngine.userOnTeam(authKey)) {


                            int queryStatus = Launcher.dbEngine.removeUserFromTeam(teamId,authKey);

                            if(queryStatus == 1) {
                                responseMap.put("success", Boolean.TRUE.toString());
                                responseMap.put("success_desc", "removed from team");
                            } else if(queryStatus == -2) {
                                responseMap.put("success", Boolean.FALSE.toString());
                                responseMap.put("status_desc","Constraint Violation Exception");
                            } else if(queryStatus == -1) {
                                responseMap.put("success", Boolean.FALSE.toString());
                                responseMap.put("status_desc","Unknown SQL Error");
                            }


                    } else {
                        responseMap.put("success", Boolean.FALSE.toString());
                        responseMap.put("status_desc","User not currently assigned to team '" + teamId + "'");
                    }

                } else {
                    responseMap.put("success", Boolean.FALSE.toString());
                    responseMap.put("status_desc","Team does not exist!");
                }

            } else {
                responseMap.put("success", Boolean.FALSE.toString());
                responseMap.put("status_desc","User does not exist!");
            }


            responseString = Launcher.gson.toJson(responseMap);

        } catch (Exception ex) {

            StringWriter sw = new StringWriter();
            ex.printStackTrace(new PrintWriter(sw));
            String exceptionAsString = sw.toString();
            ex.printStackTrace();

            return Response.status(500).entity(exceptionAsString).build();
        }
        return Response.ok(responseString).header("Access-Control-Allow-Origin", "*").build();
    }

    @GET
    @Path("/myteam")
    @Produces(MediaType.APPLICATION_JSON)
    public Response myTeam(@HeaderParam("X-Auth-API-Key") String authKey) {
        String responseString = "{}";
        try {

            Map<String, String> responseMap = new HashMap<>();

            responseMap.put("user_id", authKey);

            if (Launcher.dbEngine.userExist(authKey)) {


                String myTeamId = Launcher.dbEngine.getMyTeamId(authKey);

                if(myTeamId != null) {
                    responseMap.put("team_id",myTeamId);
                    responseMap.put("success", Boolean.TRUE.toString());
                    responseMap.put("success_desc", "user is part of a team");
                } else {
                    responseMap.put("success", Boolean.FALSE.toString());
                    responseMap.put("status_desc", "User not part of a team");
                }



            } else {
                responseMap.put("success", Boolean.FALSE.toString());
                responseMap.put("status_desc","User does not exist!");
            }


            responseString = Launcher.gson.toJson(responseMap);

        } catch (Exception ex) {

            StringWriter sw = new StringWriter();
            ex.printStackTrace(new PrintWriter(sw));
            String exceptionAsString = sw.toString();
            ex.printStackTrace();

            return Response.status(500).entity(exceptionAsString).build();
        }
        return Response.ok(responseString).header("Access-Control-Allow-Origin", "*").build();
    }

    @POST
    @Path("/add")
    @Consumes(MediaType.APPLICATION_JSON)
    public Response crunchifyREST11(InputStream incomingData) {

        StringBuilder crunchifyBuilder = new StringBuilder();
        String returnString = null;
        try {

            BufferedReader in = new BufferedReader(new InputStreamReader(incomingData));
            String line = null;
            while ((line = in.readLine()) != null) {
                crunchifyBuilder.append(line);
            }

            String jsonString = crunchifyBuilder.toString();
            returnString = jsonString;

            //return Response.ok(qname).header("Access-Control-Allow-Origin", "*").build();
        } catch (Exception ex) {

            StringWriter sw = new StringWriter();
            ex.printStackTrace(new PrintWriter(sw));
            String exceptionAsString = sw.toString();
            ex.printStackTrace();

            return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity("Internal Server Error")
                    .header("Access-Control-Allow-Origin", "*").build();
        }

        // return HTTP response 200 in case of success
        //return Response.status(200).entity("woot2").build();
        //return Response.ok(returnString, MediaType.APPLICATION_JSON_TYPE).build();
        return Response.ok(returnString).header("Access-Control-Allow-Origin", "*").build();
    }

}
