package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.RoleAccessDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.concurrent.CompletableFuture;

public interface IRoleAccess {
    ResponseSuccess save(String roleName, String accessName, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(String roleName, String accessName, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseDelete> delete(String roleName,String accessName, String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<Page<RoleAccessDTO>> list(String roleName, String accessName, String sort, String sortColumn, Integer pageNumber,
                             Integer pageSize);
    CompletableFuture<Page<RoleAccessDTO>> listFalse(String roleName, String accessName, String sort, String sortColumn, Integer pageNumber,
                             Integer pageSize);
    CompletableFuture<ResponseSuccess> activate(String roleName,String accessName, String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
}
