package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.DistrictDTO;
import com.proyect.masterdata.dto.request.RequestDistrict;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IDistrict {
        ResponseSuccess save(String name, String user, String province)
                        throws BadRequestExceptions, InternalErrorExceptions;

        ResponseSuccess saveAll(List<String> names, String user, String province)
                        throws BadRequestExceptions, InternalErrorExceptions;

        DistrictDTO update(RequestDistrict requestDistrict) throws BadRequestExceptions, InternalErrorExceptions;

        ResponseDelete delete(Long code, String user) throws BadRequestExceptions, InternalErrorExceptions;

        List<DistrictDTO> listDistrict() throws BadRequestExceptions;

        Page<DistrictDTO> list(String name, String user, Long codeProvince, String nameProvince, String sort,
                        String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;

        Page<DistrictDTO> listStatusFalse(String name, String user, Long codeProvince, String nameProvince, String sort,
                        String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;

        DistrictDTO findByCode(Long code) throws BadRequestExceptions;

        List<DistrictDTO> listDistrictByProvince(String province) throws InternalErrorExceptions, BadRequestExceptions;
}
