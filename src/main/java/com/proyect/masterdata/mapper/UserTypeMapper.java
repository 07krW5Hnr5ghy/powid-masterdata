package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.Department;
import com.proyect.masterdata.domain.UserType;
import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.UserTypeDTO;
import com.proyect.masterdata.dto.request.RequestUserTypeSave;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;
import java.util.List;

@Mapper(componentModel = "spring")
public interface UserTypeMapper {
    DepartmentMapper INSTANCE = Mappers.getMapper(DepartmentMapper.class);

    @Mapping(target = "id", ignore = true)
    @Mapping(target = "status", constant = "true")
    @Mapping(target = "dateRegistration", ignore = true)
    @Mapping(target = "userType", source = "requestUserTypeSave.userType")
    @Mapping(target = "user", source = "requestUserTypeSave.user")
    UserType userTypeToName(RequestUserTypeSave requestUserTypeSave);

    List<UserType> listUserTypeToListName(List<RequestUserTypeSave> requestUserTypeSaves);

    @Mapping(target = "code", source = "id")
    UserTypeDTO userTypeToUserTypeDTO(UserType userType);

    List<UserTypeDTO> listuserTypeToListuserTypeDTO(List<UserType> userTypeList);

}
