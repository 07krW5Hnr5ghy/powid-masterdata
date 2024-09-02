package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestClientSave {
    String name;
    String surname;
    String ruc;
    String dni;
    String business;
    String mobile;
    String address;
    String email;
    String province;
    String district;
}
