package com.proyect.masterdata.dto.request;

import jakarta.persistence.Entity;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestCustomer {
    private String name;
    private String type;
    private String instagram;
    private String phone;
    private String address;
    private String reference;
    private String district;
    private String province;
    private String department;
}
