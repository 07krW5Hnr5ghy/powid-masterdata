package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.MembershipTypeDTO;
import com.proyect.masterdata.dto.request.RequestDepartment;
import com.proyect.masterdata.dto.request.RequestMembershipType;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;

import java.util.List;

public interface IMembershipType {
    ResponseSuccess save(String name) throws BadRequestExceptions;
    ResponseSuccess saveAll(List<String> names) throws BadRequestExceptions;
    MembershipTypeDTO update(RequestMembershipType requestMembershipType) throws BadRequestExceptions;
    ResponseDelete delete(Long code) throws BadRequestExceptions;
    ResponseDelete deleteAll(List<Long> codes) throws BadRequestExceptions;
    List<MembershipTypeDTO> list() throws BadRequestExceptions;
    MembershipTypeDTO findByCode(Long code) throws BadRequestExceptions;
    MembershipTypeDTO findByName(String name) throws BadRequestExceptions;
}
