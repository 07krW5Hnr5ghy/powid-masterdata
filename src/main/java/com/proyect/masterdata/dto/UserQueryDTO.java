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
public class UserQueryDTO {
    private String dni;
    private String user;
    private String name;
    private String surname;
    private String email;
    private String district;
    private String address;
    private String mobile;
    private String gender;
}
