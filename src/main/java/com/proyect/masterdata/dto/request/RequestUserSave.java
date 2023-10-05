package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.sql.Date;

@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class RequestUserSave {
    private String user;
    private String name;
    private String surname;
    private String dni;
    private String email;
    private String address;
    private String gender;
    private String mobile;
    private String password;
    private Long id_district;
    private Long idUserType;
    private Date dateRegistration;
    private Long status;
}
