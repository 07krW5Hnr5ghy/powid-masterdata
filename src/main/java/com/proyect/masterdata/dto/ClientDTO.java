package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ClientDTO {
    String name;
    String surname;
    String ruc;
    String dni;
    String business;
    String mobile;
    String address;
    String email;
    String district;
    Boolean status;
}
