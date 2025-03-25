package com.proyect.masterdata.dto.request;


import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestCourierUser {
    //courier date
    //private String courier; // name + surname <- user
    private String company;
    //private String phone; //mobile <- user
    //private String address; <- user
    private String plate;

    //user date
    private String dni;
    private String username;
    private String name;
    private String surname;
    private String email;
    private String password;
    private String district;
    private String address;
    private String mobile;
    private String gender;
    private Boolean status;
}
