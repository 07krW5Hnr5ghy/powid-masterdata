package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestOnboarding {

    private String username;

    private String name;

    private String surname;

    private String email;

    private String address;

    private String phoneNumber;

    private String dni;

    private String bussinesName;

    private String bussinesRuc;

    private String gender;

    private String password;

    private String district;

}
