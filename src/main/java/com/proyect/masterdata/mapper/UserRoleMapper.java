package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.UserRole;
import com.proyect.masterdata.dto.UserRoleDTO;
import com.proyect.masterdata.dto.request.RequestUserRole;
import com.proyect.masterdata.dto.request.RequestUserRoleSave;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface UserRoleMapper {
    UserRoleMapper INSTANCE = Mappers.getMapper(UserRoleMapper.class);
    @Mapping(target = "code", source = "id")
    UserRoleDTO userRoleToUserRoleDTO(UserRole userRole);
    List<UserRoleDTO> listUserRoleToListUserRoleDTO(List<UserRole> userRoleList);
    @Mapping(target = "id", ignore = true)
    @Mapping(target = "status", constant = "true")
    @Mapping(target = "dateRegistration", ignore = true)
    @Mapping(target = "name", source = "name")
    UserRole userRoleToName(String name,String user);

    @Mapping(target = "id", source = "code")
    @Mapping(target = "dateRegistration", ignore = true)
    UserRole requestUserRoleToUserRole(RequestUserRole requestUserRole);

    List<UserRole> listUserRoleToListName(List<RequestUserRoleSave> requestUserRoleSaveList);
}
