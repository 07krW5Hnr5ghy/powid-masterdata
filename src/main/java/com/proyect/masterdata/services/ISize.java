package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.SizeDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface ISize {
        ResponseSuccess save(String name, String sizeType, String tokenUser)
                        throws BadRequestExceptions, InternalErrorExceptions;

        ResponseSuccess saveAll(List<String> names, String sizeType, String tokenUser)
                        throws BadRequestExceptions, InternalErrorExceptions;

        ResponseDelete delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;

        List<SizeDTO> listSize() throws BadRequestExceptions;

        Page<SizeDTO> list(String name, String user, String sort,
                        String sortColumn,
                        Integer pageNumber, Integer pageSize) throws BadRequestExceptions;

        Page<SizeDTO> listStatusFalse(String name, String user, String sort,
                        String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;

        List<SizeDTO> findAllSizeTypeName(String nameSizeType) throws BadRequestExceptions;
}
