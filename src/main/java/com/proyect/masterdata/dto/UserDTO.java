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
    String username;
    String name;
    String surname;
    String email;
    String password;
    String district;
    String address;
    String phoneNumber;
    String userType;
    String gender;
    String[] modules;
    Boolean status;
}
