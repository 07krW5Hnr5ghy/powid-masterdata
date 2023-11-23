package com.proyect.masterdata.security;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Set;
import java.util.List;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Access;
import com.proyect.masterdata.domain.Role;

import lombok.AllArgsConstructor;

@AllArgsConstructor
public class SecurityUser implements UserDetails {

    private final User user;

    @Override
    public Collection<? extends GrantedAuthority> getAuthorities() {

        Set<Role> roles = user.getRoles();
        List<GrantedAuthority> authoritiesList = new ArrayList<>();

        for (Role role : roles) {

            authoritiesList.add(new SimpleGrantedAuthority("ROLE:" + role.getName()));

            Set<Access> accesses = role.getAccesses();

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
