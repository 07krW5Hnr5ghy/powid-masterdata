package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestSupplier {
    private String businessName;
    private String ruc;
    private String country;
    private String location;
    private String phone;
    private String email;
    private String supplierType;
    private String district;
}
