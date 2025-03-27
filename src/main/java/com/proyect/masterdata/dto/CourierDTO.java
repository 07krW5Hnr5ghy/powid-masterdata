package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class CourierDTO {
    private String name;
    private String phone;
    private String address;
    private String plate;
    private String company;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
    private UUID id;
    private String user;
    private Boolean status;
    private String dni;
}
