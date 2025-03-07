package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.Role;
import com.proyect.masterdata.dto.RoleDTO;
import com.proyect.masterdata.dto.request.RequestRoleSave;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface RoleMapper {
    RoleMapper INSTANCE = Mappers.getMapper(RoleMapper.class);
}
