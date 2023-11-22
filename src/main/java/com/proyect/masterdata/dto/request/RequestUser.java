package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class RequestUser {
    private String user;
    private String name;
    private String surname;
    private String dni;
    private String email;
    private String address;
    private String gender;
    private String mobile;
    private String password;
    private String district;
}
