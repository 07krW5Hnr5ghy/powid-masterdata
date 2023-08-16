package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.SaleChannelDTO;
import com.proyect.masterdata.dto.request.RequestSaleChannel;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;

import java.util.List;

public interface ISaleChannel {
    ResponseSuccess save(String name,String user) throws BadRequestExceptions;
    ResponseSuccess saveAll(List<String> names,String user) throws BadRequestExceptions;
    SaleChannelDTO update(RequestSaleChannel requestSaleChannel) throws BadRequestExceptions;
    ResponseDelete delete(Long code,String user) throws BadRequestExceptions;
    ResponseDelete deleteAll(List<Long> codes,String user) throws BadRequestExceptions;
    List<SaleChannelDTO> list() throws BadRequestExceptions;
    List<SaleChannelDTO> listStatusFalse() throws BadRequestExceptions;
    SaleChannelDTO findByCode(Long code) throws BadRequestExceptions;
    SaleChannelDTO findByName(String name) throws BadRequestExceptions;
    List<SaleChannelDTO> findByUser(String user) throws BadRequestExceptions;
}
