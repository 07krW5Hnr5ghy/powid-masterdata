package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.UserRole;
import com.proyect.masterdata.dto.MasterListDTO;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface UserRoleMapper {
    UserRoleMapper INSTANCE = Mappers.getMapper(UserRoleMapper.class);
    @Mapping(source="id",target = "id")
    @Mapping(source="name",target = "name")
    @Mapping(source = "status",target = "status")
    MasterListDTO userRoleToUserRoleDTO(UserRole userRole);
    List<MasterListDTO> userRoleListToUserRoleListDTO(List<UserRole> userRoleList);
}
