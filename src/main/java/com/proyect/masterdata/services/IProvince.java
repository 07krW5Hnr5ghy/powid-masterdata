package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.ProvinceDTO;
import com.proyect.masterdata.dto.request.RequestProvince;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IProvince {
        ResponseSuccess save(String name, String user, String department)
                        throws BadRequestExceptions, InternalErrorExceptions;
        CompletableFuture<ResponseSuccess> saveAsync(String name, String user, String department)
                throws BadRequestExceptions, InternalErrorExceptions;
        ResponseSuccess saveAll(List<String> names, String user, String department)
                        throws BadRequestExceptions, InternalErrorExceptions;
        CompletableFuture<ResponseDelete> delete(String name, String user) throws BadRequestExceptions, InternalErrorExceptions;
        CompletableFuture<List<ProvinceDTO>> listProvince() throws BadRequestExceptions;
        CompletableFuture<Page<ProvinceDTO>> list(String name, String user, Long codeDepartment, String nameDepartment, String sort,
                        String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
        CompletableFuture<Page<ProvinceDTO>> listStatusFalse(String name, String user, Long codeDepartment, String nameDepartment,
                        String sort, String sortColumn, Integer pageNumber, Integer pageSize)
                        throws BadRequestExceptions;
        CompletableFuture<List<ProvinceDTO>> listProvinceByDepartment(String department)
                        throws InternalErrorExceptions, BadRequestExceptions;
}
