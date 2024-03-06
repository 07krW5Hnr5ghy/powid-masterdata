package com.proyect.masterdata.security;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.repository.AccessRepository;
import com.proyect.masterdata.repository.RoleAccessRepository;
import com.proyect.masterdata.repository.RoleRepository;
import com.proyect.masterdata.repository.UserRoleRepository;
import lombok.AllArgsConstructor;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

@AllArgsConstructor
public class SecurityUser implements UserDetails {

    private final User user;
    private final UserRoleRepository userRoleRepository;
    private final RoleRepository roleRepository;
    private final RoleAccessRepository roleAccessRepository;
    private final AccessRepository accessRepository;

    @Override
    public Collection<? extends GrantedAuthority> getAuthorities() {

        List<UserRole> userRoles = userRoleRepository.findByUserId(user.getId());
        List<Role> roles = roleRepository
                .findAllById(userRoles.stream().map(UserRole::getRoleId).toList());
        List<GrantedAuthority> authoritiesList = new ArrayList<>();

        for (Role role : roles) {

            authoritiesList.add(new SimpleGrantedAuthority("ROLE:" + role.getName()));

            List<RoleAccess> roleAccesses = roleAccessRepository.findByRoleId(role.getId());

            List<Access> accesses = accessRepository
                    .findAllById(roleAccesses.stream().map(RoleAccess::getAccessId).toList());

            for (Access access : accesses) {
                authoritiesList.add(new SimpleGrantedAuthority("ACCESS:" + access.getName()));
            }

        }

        return authoritiesList;
    }

    @Override
    public String getPassword() {
        return user.getPassword();
    }

    @Override
    public String getUsername() {
        return user.getUsername();
    }

    @Override
    public boolean isAccountNonExpired() {
        return true;
    }

    @Override
    public boolean isAccountNonLocked() {
        return user.getStatus();
    }

    @Override
    public boolean isCredentialsNonExpired() {
        return true;
    }

    @Override
    public boolean isEnabled() {
        return user.getStatus();
    }

}
