package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestClient {
    private String name;
    private String surname;
    private String ruc;
    private String business;
    private String dni;
    private String mobile;
    private String address;
    private String email;
    private String district;
}
