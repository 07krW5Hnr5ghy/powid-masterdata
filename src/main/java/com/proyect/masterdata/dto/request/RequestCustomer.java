package com.proyect.masterdata.dto.request;

import com.proyect.masterdata.domain.CustomerType;
import com.proyect.masterdata.domain.District;
import com.proyect.masterdata.domain.User;
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
    private String customerType;
    private String instagram;
    private String phone;
    private String address;
    private String reference;
    private String district;
    private String tokenUser;
}
