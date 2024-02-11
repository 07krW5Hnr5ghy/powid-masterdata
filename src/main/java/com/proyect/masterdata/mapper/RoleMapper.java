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

    @Mapping(target = "code", source = "id")
    RoleDTO roleToRoleDTO(Role role);

    List<RoleDTO> listRoleToListRoleDTO(List<Role> roleList);

    @Mapping(target = "id", ignore = true)
    @Mapping(target = "status", constant = "true")
    @Mapping(target = "registrationDate", ignore = true)
    @Mapping(target = "name", source = "requestUserRoleSave.name")
    @Mapping(target = "tokenUser", source = "requestUserRoleSave.tokenUser")
    Role nameToRole(RequestRoleSave requestUserRoleSave);

    List<Role> listNameToListRole(List<RequestRoleSave> requestUserRoleSaveList);
}
