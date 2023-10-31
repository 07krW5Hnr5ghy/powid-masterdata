package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

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
    String district;
    String address;
    String mobile;
    String userType;
    String gender;
    Long status;
}
