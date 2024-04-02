package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class SupplierDTO {
    private String businessName;
    private String ruc;
    private String country;
    private String location;
    private String phoneNumber;
    private String email;
    private String supplierType;
    private String department;
    private String province;
    private String district;
    private Long id;
}
