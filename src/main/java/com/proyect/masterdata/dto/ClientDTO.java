package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.Date;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ClientDTO {
    private String name;
    private String surname;
    private String ruc;
    private String dni;
    private String business;
    private String mobile;
    private String address;
    private String email;
    private String district;
    private Boolean status;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
}
