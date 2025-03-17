package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class SupplierDTO {
    private String name;
    private String ruc;
    private String country;
    private String location;
    private String phone;
    private String email;
    private String supplierType;
    private String department;
    private String province;
    private String district;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
}
