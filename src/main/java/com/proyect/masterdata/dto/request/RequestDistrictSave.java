package com.proyect.masterdata.dto.request;

import com.proyect.masterdata.domain.Province;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestDistrictSave {
    private String name;
    private String user;
    private Long codeProvince;
    private Province province;
}
