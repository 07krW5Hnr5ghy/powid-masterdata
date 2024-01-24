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
    private String dni;
    private String username;
    private String name;
    private String surname;
    private String email;
    private String password;
    private String district;
    private String address;
    private String mobile;
    private String userType;
    private String gender;
    private Boolean status;
}
