package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class UserDTO {
    String dni;
    String user;
    String name;
    String surname;
    String email;
    String password;
    Long district;
    String address;
    String mobile;
    Long userType;
    String gender;
    String[] modules;
    Long status;
}
