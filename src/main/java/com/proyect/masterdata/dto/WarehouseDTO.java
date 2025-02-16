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
public class WarehouseDTO {
    private String name;
    private String contact;
    private String phone;
    private String address;
    private String reference;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
}
