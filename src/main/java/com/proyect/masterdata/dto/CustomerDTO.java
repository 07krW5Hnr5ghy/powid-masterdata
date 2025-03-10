package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class CustomerDTO {
    private String name;
    private String instagram;
    private String phone;
    private String address;
    private String customerType;
    private String district;
    private String department;
    private String province;
    private UUID id;
    private String user;
    private Boolean status;
    private  String dni;
}
