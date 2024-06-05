package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.DistrictDTO;
import com.proyect.masterdata.dto.request.RequestDistrict;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IDistrict {
        ResponseSuccess save(String name, String user, String province)
                        throws BadRequestExceptions, InternalErrorExceptions;
        CompletableFuture<ResponseSuccess> saveAsync(String name,String user,String province) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<ResponseDelete> delete(String name, String user) throws BadRequestExceptions, InternalErrorExceptions;
        CompletableFuture<ResponseSuccess> activate(String name, String user) throws BadRequestExceptions, InternalErrorExceptions;
        CompletableFuture<List<DistrictDTO>> listDistrict() throws BadRequestExceptions;
        CompletableFuture<Page<DistrictDTO>> list(String name, String user, Long codeProvince, String nameProvince, String sort,
                        String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
        CompletableFuture<Page<DistrictDTO>> listStatusFalse(String name, String user, Long codeProvince, String nameProvince, String sort,
                        String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
        CompletableFuture<List<DistrictDTO>> listDistrictByProvince(String province) throws InternalErrorExceptions, BadRequestExceptions;
}
